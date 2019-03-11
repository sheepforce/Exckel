module Exckel.CubeGenerator.Exckel
( writeCube
) where
import           Data.Array.Repa           ((:.), Array, DIM3, U)
import qualified Data.Array.Repa           as R
import qualified Data.Array.Repa.Eval      as R
import           Data.Attoparsec.Text.Lazy
import           Data.Either               hiding (fromRight)
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Data.Vector               as V
import           Exckel.ExcUtils
import           Exckel.Parser
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read

fromRight :: Either a b -> b
fromRight (Right x) = x

-- | Write a Gaussian cube file. Everything in Bohr.
writeCube :: Cube -> T.Text
writeCube c = T.unlines
  [ -- comment lines
    T.pack $ c ^. comment

    -- number of atoms and origin vector of the cell
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (length $ c ^. atoms)
               (c ^. volumeOrigin . _1)
               (c ^. volumeOrigin . _2)
               (c ^. volumeOrigin . _3)

    -- number of voxels in A vector direction (usually x) and A vector
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (c ^. voxelDimension . _1)
               (c ^. volumeVectorA . _1)
               (c ^. volumeVectorA . _2)
               (c ^. volumeVectorA . _3)

    -- number of voxels in B vector direction (usually y) and B vector
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (c ^. voxelDimension . _2)
               (c ^. volumeVectorB . _1)
               (c ^. volumeVectorB . _2)
               (c ^. volumeVectorB . _3)
    -- number of voxels in C vector direction (usually z) and C vector
  , T.pack $ printf "%5d    %10.7f  %10.7f  %10.7f"
               (c ^. voxelDimension . _3)
               (c ^. volumeVectorC . _1)
               (c ^. volumeVectorC . _2)
               (c ^. volumeVectorC . _3)
  -- atoms and volumetric data
  , T.concat $
      [ -- atoms
        T.unlines $ map (\a -> T.pack $ printf "  %3d  %10.7f    %10.7f  %10.7f  %10.7f"
                                      (a ^. atomicNumber)
                                      ((fromIntegral :: Int -> Float) $ a ^. atomicNumber)
                                      (a ^. coordinate . _1)
                                      (a ^. coordinate . _2)
                                      (a ^. coordinate . _3)
                    ) (c ^. atoms)
        -- volumetric data
      , T.concat $ map zChunkWriter zChunks
      ]
  ]
  where
    -- convert REPA to plain list and split the list into chunks of zVoxel length
    zChunks = chunksOf (c ^. voxelDimension . _3) (R.toList $ c ^. volumetricData)
    -- writes a single line of volumetric data
    lineChunkWriter lC = T.pack $ concatMap (printf "%16.7e  ") lC
    -- write a complete chunk of volumetric data belongig to the same x and y coordinates layer
    zChunkWriter zC = T.unlines . map lineChunkWriter $ chunksOf 6 zC

-- | Calculate a CDD from available orbital cubes. Returns hole, electron and CDD
calculateCDD :: FileInfo -> ExcState -> IO (Either String (Cube, Cube, Cube))
calculateCDD fi eS
  | False `elem` (map (\rO -> rO `elem` availableOrbs) $ requiredOrbs) = return $
      Left $ "Orbital cubes are missing. These are available: \n    " ++
             show availableOrbs ++
             "But you would need: \n    " ++
             show requiredOrbs
  | otherwise = do
      -- Read all cubes necessary from disk. They will be tupled with the orbital representation
      -- used in the CI determinant pairs.
      labeledOrbCubesEither <- mapM (\i -> do
                                      cubeIRaw <- T.readFile $ (fi ^. outputPrefix) ++ [pathSeparator] ++ "orb" ++ show i ++ ".cube"
                                      let cubeI = parseOnly cube cubeIRaw
                                      return (orbNumber2Orb i nMOs isOpenShell, cubeI)
                                    ) requiredOrbs
      let -- After checking (in the return at the end of the function), if all cubes parsed
          -- successfully, strip the Either away from it
          labeledOrbCubes = map (\(o, c) -> (o, fromRight c)) labeledOrbCubesEither
          -- The volumeOrigin, voxelDimension and the volumeOrigins all need to be the same for the
          -- following calculations to be valid. The "fromRight" action is only safe, because of
          -- laziness of the return of results from this function at the end.
          cubeCharacteristics =
            map (\c -> ( c ^. volumeOrigin
                       , c ^. voxelDimension
                       , c ^. volumeVectorA
                       , c ^. volumeVectorB
                       , c ^. volumeVectorC
                       )
                ) .
            map snd $ labeledOrbCubes
          -- For some shared data such as REPA shapes and atoms, take them from the first cube found
          fstOrbCube = snd . head $ labeledOrbCubes
          cubeAtoms = fstOrbCube ^. atoms
          cubeShape = R.extent $ fstOrbCube ^. volumetricData
          -- Create the summation neutral cube (exactly as same as the original cube) with 0 as
          -- every element. Relevant point here, is, that it is a delayed cube.
          sumNeutralDRepa =
            R.map (* 1.0) $
            R.fromListUnboxed
              (cubeShape :: DIM3)
              (replicate
                ((fstOrbCube ^. voxelDimension . _1) * (fstOrbCube ^. voxelDimension . _2) * (fstOrbCube ^. voxelDimension . _3))
                0.0
              )
          -- The labeled cubes are now multiplied with the weights of the holes of the excited state
          weightedHoleDensities =
            [ R.map (* (fst orb)) .           -- The orbital is multiplied by its weight
              R.map (** 2.0) .                -- The orbital cube is squared to obtain a density
              (^. volumetricData) .           -- Take only the volumetric data from the cube
              snd .                           -- Take the cube file out of the filtered result list
                                              -- and drop the labeling
              head .                          -- Take only one of them (shoul only have one but for
                                              -- sure reduces to single element)
              filter (\a -> fst a == snd orb) -- For the current orbital (orb) to take into account,
                                              -- look for the corresponding orbital cube
              $ labeledOrbCubes               -- These are the orbital volumetric cube data labeled
                                              -- with their orbital representation
            | orb <- weightedHoleOrbs
            ]
          -- Sum up all cubes with their weights to build the overall hole density.
          holeDensityDelayed =
            foldl (R.+^) sumNeutralDRepa weightedHoleDensities
          -- Same procedure for the electron densities.
          weightedElectronDensities =
            [ R.map (* (fst orb)) .
              R.map (** 2.0) .
              (^. volumetricData) .
              snd .
              head .
              filter (\a -> fst a == snd orb)
              $ labeledOrbCubes
            | orb <- weightedElectronOrbs
            ]
          electronDensityDelayed =
            foldl (R.+^) sumNeutralDRepa weightedElectronDensities
      -- Compute the values in parallel
      holeDensity <- R.computeP holeDensityDelayed
      electronDensity <- R.computeP electronDensityDelayed
      chargeDensityDifference <- R.computeP $ electronDensityDelayed R.-^ holeDensityDelayed
      -- Build cubes again with the volumetric data
      let holeCube = fstOrbCube
            & volumetricData .~ holeDensity
            & comment .~ ("Hole density of state " ++ show (eS ^. nState)) ++ "\ngenerated by Exckel"
          electronCube = fstOrbCube
            & volumetricData .~ electronDensity
            & comment .~ ("Electron density of state " ++ show (eS ^. nState)) ++ "\ngenerated by Exckel"
          cddCube = fstOrbCube
            & volumetricData .~ chargeDensityDifference
            & comment .~ ("Charge density difference of state " ++ show (eS ^. nState)) ++ "\ngenerated by Exckel"
      -- Check if all cubes could be parsed
      if False `elem` map (isRight . snd) labeledOrbCubesEither
        -- If there are some that could not be parsed, calculation can not be done
        then return $ Left "Some of your cube files could not be parsed."
        -- Ff all of them parsed, check if they are all compatible
        else if all (== head cubeCharacteristics) cubeCharacteristics
          then return $ Right (holeCube, electronCube, cddCube)
          else return $ Left "Your cubes are not compatible with each other. They need same grid specification but don't have."
  where
    -- Do the data come from an open shell calculation?
    isOpenShell = case eS ^. wfType of
      Nothing          -> False
      Just ClosedShell -> False
      _                -> True
    -- Number of molecular orbitals in the wavefunction
    nMOs = eS ^. nBasisFunctions
    -- Excited CI determinants constructing the excited state
    ciExcitations = eS ^. ciWavefunction
    -- All orbitals constructing the CI wavefunction
    requiredOrbs = sort . getOrbNumbers $ eS
    -- All orbitals FilePaths available on disk as cubes
    availableOrbsFiles = fromMaybe [] $ fi ^. cubeFiles . orbCubes
    -- All orbitals by their number as written to a molden file.
    availableOrbs =
      sort .
      map fromJust .
      filter isJust .
      map ((readMaybe :: String -> Maybe Int) . drop 3 . takeBaseName) $
      availableOrbsFiles
    -- Take a CI determinant and reduce to weighted orbitals that form the hole. Effectively breaks
    -- down excitations higher than singles to singles.
    reduceToSinglesHole :: CIDeterminant -> [(Double, (Int, Maybe Spin))]
    reduceToSinglesHole ci = zip (repeat $ ci ^. weight) (map (^. fromOrb) $ ci ^. excitationPairs)
    -- Same for the electrons
    reduceToSinglesElectron :: CIDeterminant -> [(Double, (Int, Maybe Spin))]
    reduceToSinglesElectron ci = zip (repeat $ ci ^. weight) (map (^. toOrb) $ ci ^. excitationPairs)
    -- Orbital excitation pairs (orbital from & orbital to) are now isolated if this is a higher
    -- excitation than a singles. So a double excitation will now be two entries with same weight
    -- but overall, everything is now reduced to formally a singles excitation.
    weightedHoleOrbs = concat . V.toList . V.map reduceToSinglesHole $ ciExcitations
    weightedElectronOrbs = concat .V.toList . V.map reduceToSinglesElectron $ ciExcitations
