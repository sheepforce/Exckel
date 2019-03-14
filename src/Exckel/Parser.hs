{-
Contains all parsers for quantum chemistry software outputs. Contains also parsers for
-}
{-# LANGUAGE OverloadedStrings #-}
module Exckel.Parser
( gaussianLogTDDFT
, nwchemTDDFT
, mrccADC
, cube
, vmdState
, vmdRC
) where
import           Control.Applicative
import           Data.Array.Repa      (Array, DIM3, U, Z)
import qualified Data.Array.Repa      as R
import           Data.Attoparsec.Text hiding (take)
import           Data.Sort
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Exckel.ExcUtils
import           Exckel.Types
import           Lens.Micro.Platform
import           Prelude              hiding (takeWhile)
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

-- | Parse NWChem log file from TDDFT output calculation (RPA) for open shell and closed shell.
nwchemTDDFT :: Parser [ExcState]
nwchemTDDFT = do
  -- wavefunction type
  _ <- manyTill anyChar (string "Wavefunction type:")
  _ <- takeWhile isHorizontalSpace
  wfString <- string "spin polarized." <|> string "closed shell."
  let wfType' = case wfString of
        "spin polarized." -> Just OpenShell
        "closed shell."   -> Just ClosedShell
        _                 -> Nothing
  -- multiplicity
  _ <- manyTill anyChar (string "Spin multiplicity:")
  _ <- takeWhile isHorizontalSpace
  multiplicity' <- decimal
  -- number of orbitals / basis functions. I am not absolutely sure, that this works if linear
  -- dependences are removed
  _ <- manyTill anyChar (string "AO basis - number of functions:")
  _ <- takeWhile isHorizontalSpace
  nBasisFunctions' <- decimal
  -- parse all excited state blocks
  states' <- many1 $ do
    -- begin of an excitation block
    _ <- manyTill anyChar (string "Root")
    -- number of the excited state
    _ <- takeWhile isHorizontalSpace
    nState' <- decimal
    -- potentially the multiplicity in closed shell calculation
    _ <- takeWhile isHorizontalSpace
    _ <- option Nothing (Just <$> (string "singlet" <|> string "triplet"))
    -- symmetry label
    _ <- takeWhile isHorizontalSpace
    _ <- many1 (letter <|> digit)
    -- energy in hartree
    _ <- takeWhile isHorizontalSpace
    relEnergy' <- double
    _ <- takeWhile isHorizontalSpace
    _ <- string "a.u."
    -- energy in eV
    _ <- manyTill anyChar endOfLine
    -- S**2 value
    s2' <- option Nothing (Just <$> do
        _ <- takeWhile isHorizontalSpace
        _ <- string "<S2> ="
        _ <- takeWhile isHorizontalSpace
        ssquare <- double
        return ssquare
        )
    -- oscillator strength
    oscillatorStrength' <-
      -- open shell calculation
      (do
        _ <- manyTill anyChar (string "Total Oscillator Strength")
        _ <- takeWhile isHorizontalSpace
        fOsc <- double
        return fOsc
      )
      <|>
      -- closed shell calculation
      (do
        _ <- manyTill anyChar (string "Dipole Oscillator Strength")
        _ <- takeWhile isHorizontalSpace
        fOsc <- double
        return fOsc
      )
    -- CI determinant block
    skipSpace
    ciWavefunction' <- many1 $ do
      -- from orbital
      _ <- takeWhile isHorizontalSpace
      fromVirtOcc <- string "Occ." <|> string "Virt."
      _ <- takeWhile isHorizontalSpace
      fromOrbI' <- decimal
      _ <- takeWhile isHorizontalSpace
      fromOrbS' <- option Nothing (Just <$> (string "alpha" <|> string "beta"))
      _ <- takeWhile isHorizontalSpace
      _ <- many1 (letter <|> digit)
      -- separation of blocks
      _ <- takeWhile isHorizontalSpace
      _ <- string "---"
      -- to block
      _ <- takeWhile isHorizontalSpace
      toVirtOcc <- string "Occ." <|> string "Virt."
      _ <- takeWhile isHorizontalSpace
      toOrbI' <- decimal
      _ <- takeWhile isHorizontalSpace
      toOrbS' <- option Nothing (Just <$> (string "alpha" <|> string "beta"))
      _ <- takeWhile isHorizontalSpace
      _ <- many1 (letter <|> digit)
      -- coefficient
      _ <- takeWhile isHorizontalSpace
      coeff' <- double
      _ <- takeWhile (not <$> isEndOfLine)
      endOfLine
      let (spinFrom', spinTo') = case (fromOrbS', toOrbS') of
            (Nothing, Nothing)           -> (Nothing, Nothing)
            (Just "alpha", Just "alpha") -> (Just Alpha, Just Alpha)
            (Just "beta", Just "beta")   -> (Just Beta, Just Beta)
            _                            -> (Nothing, Nothing)
      return CIDeterminant
        { _excitationPairs =
            [ OrbitalExcitation
                { _fromOrb = (fromOrbI', spinFrom')
                , _toOrb   = (toOrbI', spinTo')
                }
            ]
        , _weight = (coeff')**2.0
        }
    return ExcState
      { _nState             = nState'
      , _multiplicity       = multiplicity'
      , _wfType             = wfType'
      , _s2                 = s2'
      , _relEnergy          = relEnergy'
      , _oscillatorStrength = oscillatorStrength'
      , _ciWavefunction     = V.fromList ciWavefunction'
      , _nBasisFunctions    = nBasisFunctions'
      }
  return states'

-- | Parse mrcc output of an ADC2 calculation. They can be closed shell only, currently i think
mrccADC :: Parser [ExcState]
mrccADC = do
  -- Multiplicity
  _ <- manyTill anyChar (string "Spin multiplicity:")
  _ <- takeWhile isHorizontalSpace
  multiplicityGroundState <- decimal
  -- wavefunction type
  wfString <- manyTill anyChar (string "hf calc")
  let wfType' = case (reverse . take 3 . reverse . T.words . T.pack $ wfString) of
        ["restricted", "closed", "shell"] -> Just ClosedShell
        [_, _, "unrestricted"]            -> Just OpenShell
        _                                 -> Nothing
  -- number of basis functions
  _ <- manyTill anyChar (string "Total number of basis functions:")
  _ <- takeWhile isHorizontalSpace
  nBasisFunctions' <- decimal
  return $ T.pack . show $ nBasisFunctions'
  -- excited states
  cisOrderedStates <- many1 $ do
    {-
    -- Beginning of an excited state block. Unfortunately, this occurs 2 times for each state. Once
    -- for the MP2 part and once for ADC(2). The parser is very exact here and includes all the
    -- uninteresting parts, to make sure to not match MP2 blocks but only the ADC(2) blocks.
    _ <- manyTill anyChar (string "Final result in atomic units for root")
    _ <- takeWhile isHorizontalSpace
    nStateCIS <- decimal
    _ <- char ':'
    skipSpace
    _ <- takeWhile isHorizontalSpace
    _ <- string "Total Hartree-Fock energy:"
    _ <- takeWhile isHorizontalSpace
    _ <- double
    endOfLine
    _ <- takeWhile isHorizontalSpace
    _ <- string "Total ADC(2) energy:"
    _ <- takeWhile isHorizontalSpace
    _ <- double
    endOfLine
    _ <- takeWhile isHorizontalSpace
    _ <- string "ADC(2) excitation energy:"
    -}
    -- parsing with "Final result in atomic units for root" leads to the parser entering a "many1"
    -- loop, i think, which can not be satisfied. Therefore only look for "ADC(2) excitation energy"
    _ <- manyTill anyChar (string "ADC(2) excitation energy:")
    _ <- takeWhile isHorizontalSpace
    relEnergy' <- double
    endOfLine
    _ <- takeWhile isHorizontalSpace
    _ <- string "Spin multiplicity:"
    _ <- takeWhile isHorizontalSpace
    multiplicity' <- decimal
    skipSpace
    _ <- string "Dominant coefficients"
    skipSpace
    _ <- string "Printing threshold:"
    _ <- takeWhile isHorizontalSpace
    _ <- double
    skipSpace
    _ <- string "coeff.   occ.       virt."
    skipSpace
    ciDeterminants' <- many1 $ do
      _ <- takeWhile isHorizontalSpace
      coeff' <- double
      _ <- takeWhile isHorizontalSpace
      fromOrbI' <- decimal
      _ <- takeWhile isHorizontalSpace
      _ <- string "-->"
      _ <- takeWhile isHorizontalSpace
      toOrbI' <- decimal
      _ <- takeWhile isHorizontalSpace
      endOfLine
      return CIDeterminant
        { _excitationPairs =
            [ OrbitalExcitation
                { _fromOrb = (fromOrbI', Nothing)
                , _toOrb   = (toOrbI', Nothing)
                }
            ]
        , _weight = (coeff')**2.0
        }
    return ExcState
      { _nState             = undefined
      , _multiplicity       = multiplicity'
      , _wfType             = wfType'
      , _s2                 = Nothing
      , _relEnergy          = relEnergy'
      , _oscillatorStrength = undefined
      , _ciWavefunction     = V.fromList ciDeterminants'
      , _nBasisFunctions    = nBasisFunctions'
      }
  -- MRCC has the excited state oscillator strength not together with the excited states, but at
  -- at the end of the output file. At the end also the correct order of the excited states is
  -- given. Previously, this has only been the CIS ordering of states, not the ADC(2) ordering.
  _ <- manyTill anyChar (string "Calculate oscillator and rotational strength...")
  _ <- manyTill anyChar (string "===========================================================================================")
  endOfLine
  adc2StateFosc <- many1 $ do
    _ <- takeWhile isHorizontalSpace
    adc2State <- decimal
    _ <- takeWhile isHorizontalSpace
    cisState <- decimal
    _ <- takeWhile isHorizontalSpace
    _ <- double -- x component of dipole
    _ <- takeWhile isHorizontalSpace
    _ <- double -- y component of dipole
    _ <- takeWhile isHorizontalSpace
    _ <- double -- z component of dipole
    _ <- takeWhile isHorizontalSpace
    _ <- double -- total dipole strength
    _ <- takeWhile isHorizontalSpace
    adc2Fosc <- double
    _ <- takeWhile isHorizontalSpace
    endOfLine
    return (adc2State, adc2Fosc)
  let energySortedADC2States = sortBy compareExcState cisOrderedStates
      correctADC2states =
        zipWith (\eS (adc2N, adc2F) -> (eS & nState .~ adc2N & oscillatorStrength .~ adc2F))
          energySortedADC2States adc2StateFosc
  return correctADC2states
  where
    compareExcState a b = (a ^. relEnergy) `compare` (b ^. relEnergy)

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
    , _volumeOrigin   = volumeOriginInBohr
    , _voxelDimension = (abs nVoxelsX, abs nVoxelsY, abs nVoxelsZ)
    , _volumeVectorA  = volumeVectorAInBohr
    , _volumeVectorB  = volumeVectorBInBohr
    , _volumeVectorC  = volumeVectorCInBohr
    , _comment        = (T.unpack commentLine1) ++ "\n" ++ (T.unpack commentLine2)
    , _atoms          = atomsInBohr
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
