{-
Contains all parsers for quantum chemistry software outputs. Contains also parsers for
-}
{-# LANGUAGE OverloadedStrings #-}
module Exckel.Parser
( gaussianLogTDDFT
, nwchemTDDFT
, vmdState
, vmdRC
) where
import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.Text            as T
import qualified Data.Vector          as V
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

nwchemTDDFT :: Parser [ExcState]
nwchemTDDFT = do
  -- wavefunction type
  _ <- manyTill anyChar (string "Wavefunction type:")
  _ <- takeWhile isHorizontalSpace
  wfString <- string "spin polarized." <|> string "closed shell."
  let wfType' = case wfString of
        "spin polarized." -> Just OpenShell
        "closed shell." -> Just ClosedShell
        _ -> Nothing
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
            (Nothing, Nothing)   -> (Nothing, Nothing)
            (Just "alpha", Just "alpha") -> (Just Alpha, Just Alpha)
            (Just "beta", Just "beta")   -> (Just Beta, Just Beta)
            _                            -> (Nothing, Nothing)
      return CIDeterminant
        { _excitationPairs =
            [ OrbitalExcitation
                { _fromOrb = (fromOrbI', spinFrom')
                , _toOrb = (toOrbI', spinTo')
                }
            ]
        , _weight = (coeff')**2.0
        }
    return ExcState
      { _nState = nState'
      , _multiplicity = multiplicity'
      , _wfType = wfType'
      , _s2 = s2'
      , _relEnergy = relEnergy'
      , _oscillatorStrength = oscillatorStrength'
      , _ciWavefunction = V.fromList ciWavefunction'
      , _nBasisFunctions = nBasisFunctions'
      }
  return states'

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
