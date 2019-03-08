{-
Contains all parsers for quantum chemistry software outputs. Contains also parsers for
-}
module Exckel.Parser
( gaussianLogTDDFT
, cube
, vmdState
, vmdRC
) where
import           Control.Applicative
import           Data.Array.Repa      (Array, DIM3, U, Z)
import qualified Data.Array.Repa      as R
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Exckel.ExcUtils
import           Exckel.Types
import           Lens.Micro.Platform
import           Prelude              hiding (take, takeWhile)
import           Text.Printf

-- | From the whole Gaussian TDDFT output, parse all single excited state and skip over all other
-- | parts
gaussianLogTDDFT :: Parser [ExcState]
gaussianLogTDDFT = do
  -- multiplicity
  _ <- manyTill anyChar (string "Multiplicity")
  _ <- string " = "
  multiplicity' <- decimal
  -- look for number of basis functions as NBsUse=
  _ <- manyTill anyChar (string "NBsUse=")
  _ <- takeWhile isHorizontalSpace
  nBasisFunctions' <- decimal
  -- look for wavefunction type
  wfString <- manyTill anyChar (string "shell SCF")
  let wfType' = case (T.words . last . T.lines . T.pack $ wfString) of
       ["UHF", "open"] -> Just OpenShell
       ["Closed"]      -> Just ClosedShell
       _               -> Nothing
  -- parse all excited state blocks
  states' <- many1 $ do
    -- starting string "Excited State   x:"
    _ <- manyTill anyChar (string "Excited State ")
    skipSpace
    nState' <- decimal
    _ <- char ':'
    skipSpace
    -- spin and symmetry label "x.xxx-A" or something like "Singlet-A"
    _ <- manyTill anyChar (char ' ')
    {-
    _ <- (show <$> double) <|> (many1 letter)
    _ <- char '-'
    _ <- many1 (letter <|> digit)
    -}
    skipSpace
    -- energy in eV "x.xxxx eV"
    energyElectronVolt' <- double
    skipSpace
    _ <- string "eV"
    skipSpace
    -- wavelength in nm "xxxx.xx nm"
    _ <- double
    _ <- char ' '
    _ <- string "nm"
    skipSpace
    -- oscillator strength "f=x.xxxx"
    _ <- string "f="
    oscillatorStrength' <- double
    skipSpace
    -- S**2 value "<S**2>=x.xxx"
    _ <- string "<S**2>="
    s2' <- double
    endOfLine
    -- the excitation pairs for CIS vectors
    ciWavefunction' <- many1 $ do
      _ <- takeWhile isHorizontalSpace
      fromOrbI' <- decimal
      fromOrbS' <- option Nothing (Just <$> (char 'A' <|> char 'B'))
      _ <- takeWhile isHorizontalSpace
      excDirection <- string "->" <|> string "<-"
      _ <- takeWhile isHorizontalSpace
      toOrbI' <- decimal
      toOrbS' <- option Nothing (Just <$> (char 'A' <|> char 'B'))
      _ <- takeWhile isHorizontalSpace
      coeff' <- double
      endOfLine
      let (spinFrom', spinTo') = case (fromOrbS', toOrbS') of
            (Nothing, Nothing)   -> (Nothing, Nothing)
            (Just 'A', Just 'A') -> (Just Alpha, Just Alpha)
            (Just 'B', Just 'B') -> (Just Beta, Just Beta)
            _                    -> (Nothing, Nothing)
      return CIDeterminant
        { _excitationPairs =
            case excDirection of
              "->" -> -- excitation
                [ OrbitalExcitation
                    { _fromOrb = (fromOrbI', spinFrom')
                    , _toOrb = (toOrbI', spinTo')
                    }
                ]
              "<-" -> -- back excitation
                [ OrbitalExcitation
                    { _fromOrb = (toOrbI', spinTo')
                    , _toOrb = (fromOrbI', spinFrom')
                    }
                ]
        , _weight = (coeff')**2.0
        }
    _ <- option "" $ do
      _ <- string " This state for optimization and/or second-order correction."
      _ <- takeWhile (not <$> isEndOfLine)
      endOfLine
      _ <- takeWhile (not <$> isEndOfLine)
      endOfLine
      return ""
    let weightScaleFactor = if wfType' == Just ClosedShell
          then 2.0
          else 1.0
    return ExcState
      { _nState             = nState'
      , _multiplicity       = multiplicity'
      , _wfType             = wfType'
      , _s2                 = Just s2'
      , _relEnergy          = energyElectronVolt' / 27.21138602
      , _oscillatorStrength = oscillatorStrength'
      , _ciWavefunction     = V.fromList $ map (& weight %~ (weightScaleFactor *)) ciWavefunction'
      , _nBasisFunctions    = nBasisFunctions'
      }
  return states'

-- | Parse a Gaussian cube file. Takes care of Angstrom Bohr conversion.
cube :: Parser Cube
cube = do
  -- read exactly 2 lines comments
  commentLine1 <- takeWhile (not . isEndOfLine)
  endOfLine
  commentLine2 <- takeWhile (not . isEndOfLine)
  endOfLine
  -- read number of atoms from line 3
  _ <- takeWhile isHorizontalSpace
  nAtoms <- decimal
  -- parse origin of the cell vectors
  _ <- takeWhile isHorizontalSpace
  originX <- double
  _ <- takeWhile isHorizontalSpace
  originY <- double
  _ <- takeWhile isHorizontalSpace
  originZ <- double
  _ <- takeWhile isHorizontalSpace
  endOfLine
  -- Informations about volumetric data. Number of Voxels and cell vectors are given
  voxelCellInfo <- count 3 $ do
   -- parse the voxels in direction
   _ <- takeWhile isHorizontalSpace
   nVoxels <- decimal
   -- parse the vector of the cell
   _ <- takeWhile isHorizontalSpace
   vectorX <- double
   _ <- takeWhile isHorizontalSpace
   vectorY <- double
   _ <- takeWhile isHorizontalSpace
   vectorZ <- double
   _ <- takeWhile isHorizontalSpace
   endOfLine
   return (nVoxels, (vectorX, vectorY, vectorZ))
  -- parse all atoms
  atoms' <- count nAtoms $ do
   -- read the atomic number
   _ <- takeWhile isHorizontalSpace
   aNumber <- decimal
   -- read charge ...?
   _ <- takeWhile isHorizontalSpace
   _ <- double
   -- read the atomic coordinates
   _ <- takeWhile isHorizontalSpace
   aX <- double
   _ <- takeWhile isHorizontalSpace
   aY <- double
   _ <- takeWhile isHorizontalSpace
   aZ <- double
   _ <- takeWhile isHorizontalSpace
   endOfLine
   -- return the parsed atom
   return Atom
     { _atomicNumber = aNumber
     , _coordinate = (aX, aY, aZ)
     }
  -- process parsed data
  let nVoxelsX = (voxelCellInfo !! 0) ^. _1
      nVoxelsY = (voxelCellInfo !! 1) ^. _1
      nVoxelsZ = (voxelCellInfo !! 2) ^. _1
      nVoxels = nVoxelsX * nVoxelsY * nVoxelsZ
      isInBohrX = if nVoxelsX < 0
        then False
        else True
      isInBohrY = if nVoxelsY < 0
        then False
        else True
      isInBohrZ = if nVoxelsZ < 0
        then False
        else True
      atomsInBohr = map (\a -> a
        & coordinate . _1 %~ (if isInBohrX then (* 1.0) else angstrom2Bohr)
        & coordinate . _2 %~ (if isInBohrY then (* 1.0) else angstrom2Bohr)
        & coordinate . _3 %~ (if isInBohrZ then (* 1.0) else angstrom2Bohr)
        ) atoms'
      volumeOriginInBohr =
        ( if isInBohrX then originX else angstrom2Bohr originX
        , if isInBohrY then originY else angstrom2Bohr originY
        , if isInBohrZ then originZ else angstrom2Bohr originZ
        )
      volumeVectorAInBohr = (voxelCellInfo !! 0 ^. _2)
        & _1 %~ (if isInBohrX then (* 1.0) else angstrom2Bohr)
        & _2 %~ (if isInBohrX then (* 1.0) else angstrom2Bohr)
        & _3 %~ (if isInBohrX then (* 1.0) else angstrom2Bohr)
      volumeVectorBInBohr = (voxelCellInfo !! 1 ^. _2)
        & _1 %~ (if isInBohrY then (* 1.0) else angstrom2Bohr)
        & _2 %~ (if isInBohrY then (* 1.0) else angstrom2Bohr)
        & _3 %~ (if isInBohrY then (* 1.0) else angstrom2Bohr)
      volumeVectorCInBohr = (voxelCellInfo !! 2 ^. _2)
        & _1 %~ (if isInBohrZ then (* 1.0) else angstrom2Bohr)
        & _2 %~ (if isInBohrZ then (* 1.0) else angstrom2Bohr)
        & _3 %~ (if isInBohrZ then (* 1.0) else angstrom2Bohr)
  -- Parse the volumetric data
  _ <- takeWhile isHorizontalSpace
  voxelValues <- count nVoxels $ do
    voxel <- double
    skipSpace
    return voxel
  -- return the cube
  return Cube
    { _volumetricData = R.fromListUnboxed (R.Z R.:. nVoxelsX R.:. nVoxelsY R.:. nVoxelsZ) voxelValues
    , _volumeOrigin = volumeOriginInBohr
    , _voxelDimension = (abs nVoxelsX, abs nVoxelsY, abs nVoxelsZ)
    , _volumeVectorA = volumeVectorAInBohr
    , _volumeVectorB = volumeVectorBInBohr
    , _volumeVectorC = volumeVectorCInBohr
    , _comment = (T.unpack commentLine1) ++ "\n" ++ (T.unpack commentLine2)
    , _atoms = atomsInBohr
    }

-- | take a VMD state file and parse the part of it, which gives orientation of the molecule
-- | relative to the camera (perspective)
vmdState :: Parser T.Text
vmdState = do
  _ <- manyTill anyChar (string "set viewpoints")
  viewPoint <- T.pack <$> manyTill anyChar (string "foreach v $fixedlist {")
  return viewPoint

-- | Parse a VMD rc file and return everything but the anyoing "after idle {" sourroundings of it
vmdRC :: Parser T.Text
vmdRC = do
  _ <- manyTill anyChar (string "after idle {")
  helpfulContent <- T.pack <$> manyTill anyChar (string "# </custom>")
  return helpfulContent
