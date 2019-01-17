{-# LANGUAGE OverloadedStrings #-}
module Exckel.Parser.Gaussian
( gaussianLogTDDFTSingleState
) where
import Data.Text hiding (takeWhile)
import Data.Attoparsec.Text
import Exckel.Types
import Text.Printf
import Control.Applicative
import Data.Vector hiding (takeWhile, (++))
import Prelude hiding (takeWhile)

--gaussianLogTDDFTAllStates :: [Int]

-- | From the whole Gaussian output, parse a single excited state and skip over all other parts
gaussianLogTDDFTSingleState :: Int -> Parser ExcState
gaussianLogTDDFTSingleState nState' = do
  -- starting string "Excited State   1:"
  _ <- manyTill anyChar (string . pack $ "Excited State " ++ (printf "%3d:" nState'))
  skipSpace
  -- spin and symmetry label "x.xxx-A" or something like "Singlet-A"
  _ <- (show <$> double) <|> (many1 letter)
  _ <- char '-'
  _ <- many1 (letter <|> digit)
  skipSpace
  -- energy in eV "x.xxxx eV"
  energyElectronVolt' <- double
  skipSpace
  _ <- string "eV"
  _ <- char ' '
  -- wavelength in nm "xxxx.xx nm"
  _ <- double
  _ <- char ' '
  _ <- string "nm"
  skipSpace
  -- oscillator strength "f=x.xxxx"
  _ <- string $ pack "f="
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
          (Nothing, Nothing) -> (Nothing, Nothing)
          (Just 'A', Just 'A') -> (Just Alpha, Just Alpha)
          (Just 'B', Just 'B') -> (Just Beta, Just Beta)
          _ -> (Nothing, Nothing)
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
    , _multiplicity       = Nothing
    , _wfType             = Nothing
    , _s2                 = Just s2'
    , _relEnergy          = energyElectronVolt'
    , _oscillatorStrength = oscillatorStrength'
    , _ciWavefunction     = fromList ciWavefunction'
    }
