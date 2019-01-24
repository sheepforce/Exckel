{-# LANGUAGE OverloadedStrings #-}
module Exckel.Parser
( gaussianLogTDDFT
, vmdState
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
  -- look for wavefunction type
  wfString <- manyTill anyChar (string "shell SCF")
  let wfType' = case (T.words . last . T.lines . T.pack $ wfString) of
       ["UHF", "open"] -> Just OpenShell
       ["Closed"] -> Just ClosedShell
       _ -> Nothing
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
      _ <- string "->"
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
            [ OrbitalExcitation
                { _fromOrb = (fromOrbI', spinFrom')
                , _toOrb = (toOrbI', spinTo')
                }
            ]
        , _coeff = coeff'
        }
    _ <- option "" $ do
      _ <- string " This state for optimization and/or second-order correction."
      _ <- takeWhile (not <$> isEndOfLine)
      endOfLine
      _ <- takeWhile (not <$> isEndOfLine)
      endOfLine
      return ""
    return ExcState
      { _nState             = nState'
      , _multiplicity       = Just multiplicity'
      , _wfType             = wfType'
      , _s2                 = Just s2'
      , _relEnergy          = energyElectronVolt' / 27.21138602
      , _oscillatorStrength = oscillatorStrength'
      , _ciWavefunction     = V.fromList ciWavefunction'
      }
  return states'

-- | take a VMD state file and parse the part of it, which gives orientation of the molecule
-- | relative to the camera (perspective)
vmdState :: Parser T.Text
vmdState = do
  _ <- manyTill anyChar (string "set viewpoints")
  viewPoint <- T.pack <$> manyTill anyChar (string "foreach v $fixedlist {")
  return viewPoint
