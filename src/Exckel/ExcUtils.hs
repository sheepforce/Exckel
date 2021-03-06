{-
Providing helper functions to calculate some numbers ...
-}
module Exckel.ExcUtils
( orbNumber2Orb
, orbNumberToSpinOrbNumber
, getOrbNumbers
, gauss
, convolutionSum
, oscStrength2Epsilon
, eV2nm
, nm2eV
, hartree2eV
, spinContaminationFractionOfHigherState
, filterByS2
, idealS2fromMult
, angstrom2Bohr
, bohr2Angstrom
, renumberExcitedStatesEnergy
) where
import           Data.List           (nub, sortOn)
import           Data.Maybe
import qualified Data.Vector         as V
import           Exckel.Types
import           Lens.Micro.Platform

-- | Given an orbital number (as generated by Multiwfn) and the number of basis function, convert
-- | back to the representation in excited state CI determinants.
orbNumber2Orb :: Int -> Int -> Bool ->  (Int, Maybe Spin)
orbNumber2Orb nOrb nBasFun isOpenShell = case (nOrb <= nBasFun, isOpenShell) of
  (True, False)  -> (nOrb, Nothing)
  (True, True)   -> (nOrb, Just Alpha)
  (False, True)  -> (nOrb - nBasFun, Just Beta)
  (False, False) -> error "Found more orbitals than there are basis functions but your calcualtion was closed shell."

-- | Given excited state information, this function will convert back the orbital number to orbital
-- | number + spin and make a string from it.
orbNumberToSpinOrbNumber :: Int -> Bool -> Int -> String
orbNumberToSpinOrbNumber nBasFun isOpenShell nOrb = case (nOrb <= nBasFun, isOpenShell) of
  (True, False)  -> show nOrb
  (True, True)   -> show nOrb ++ "A"
  (False, True)  -> show (nOrb - nBasFun) ++ "B"
  (False, False) -> error "Found more orbitals than there are basis functions but your calcualtion was closed shell."

-- | Get orbital indices (1 based) for an excited state as written to the wavefunction file. Beta
-- | orbitals usually start at (n(alpha orbitals) + 1).
getOrbNumbers :: ExcState -> [Int]
getOrbNumbers es = orbIndices
  where
    nBF = es ^. nBasisFunctions
    ciWF = es ^. ciWavefunction
    orbPairs = concat . V.map (^. excitationPairs) $ ciWF
    orbIndices = nub . map getSingleOrbInd $ (map (^. fromOrb) orbPairs) ++ (map (^. toOrb) orbPairs)
    getSingleOrbInd :: (Int, Maybe Spin) -> Int
    getSingleOrbInd o = case o of
      (i, Just Alpha) -> i
      (i, Just Beta)  -> nBF + i
      (i, Nothing)    -> i

-- | A gaussian line shape as defined in Gaussian whitepaper http://gaussian.com/uvvisplot/ .
-- | The definition used here differs from the Wikipedia version, in the prefactor. Here the area
-- | is not normalised but the height, so that every peak can be safely multiplied with the
-- | oscillator strength.
-- |   σ -> Standard deviation for the line width. This can be converted to FWHM by 2*sqrt(ln 2),
-- |        instead of the 2*sqrt(2 * ln 2), coming from the σ instead of σ^2, i think
-- |   fwhm -> Full width at Half Maximum, directly converted to standard deviation
-- |   eEval -> Energy in electron Volt at which to obtain the value of the broadened spectrum
-- |   ePeak -> Energy in electron Volt for the peak position (x-Coordinate)
-- |   fOsc -> Dimensionaless oscillator strength as in the QC output file
gauss :: Floating a => a -> (a, a) -> a -> a
gauss fwhm (ePeak, fOsc) eEval = fOsc * exp(- ((eEval - ePeak) / σ)**2)
  where
    σ = fwhm / (2 * sqrt(log 2))

-- | Broaden a complete stick spectrum by a given (parametrised) broadening function.
-- |   convFunc -> Function taking pairs of (peak position in electron Volt, oscillator strength)
-- |               and a grid point, on which to evaluate the value
-- |   peaks -> A list of pairs of (peak position in electron Volt, oscillator strength)
-- |   grid -> A list of grid points (x-Values in electron Volt)
-- | Result will be a list of (grid points in electron Volt, broadened oscillator strength at pos.)
convolutionSum :: Floating a => ((a, a) -> a -> a) -> [(a, a)] -> [a] -> [(a, a)]
convolutionSum convFunc peaks grid = gridResult
  where
    convFunc' grid' peaks' = convFunc peaks' grid'
    gridPointVal p = sum . map (convFunc' p) $ peaks
    gridVal = map gridPointVal grid
    gridResult = zip grid gridVal

-- | Convert from oscillator strength (dimensionless) to absorption coefficients (litre / (mol cm))
-- | The prefactor is taken from the Gaussian whitepaper, and the nominator multiplicator comes
-- | from the prefactor expecting σ to be in cm^-1, but i give it in electron Volts instead
oscStrength2Epsilon :: Floating a => a -> a -> a
oscStrength2Epsilon fwhm fOsc = 1.3062974e8 * fOsc / (σ * 8065.54400545911)
  where
    σ = fwhm / (2 * sqrt(log 2))

-- | Convert from electron Volt to nano metre
eV2nm :: Floating a => a -> a
eV2nm x = 1239.84197386209 / x

-- | Convert from nano metre to electron Volt
nm2eV :: Floating a => a -> a
nm2eV = eV2nm

-- | Convert energy from hartree to electron Volt
hartree2eV :: Floating a => a -> a
hartree2eV e = 27.21138602 * e

-- | Get the fraction of the next higher spin state contributing to the current one from spin
-- | contamination value
spinContaminationFractionOfHigherState :: Floating a => a -> a -> a
spinContaminationFractionOfHigherState pureS calcS2 =
  (calcS2 - (pureS * (pureS + 1))) /
  ((pureS + 1) * 2)

-- | Filters states by their purity of spin states. Removes all states, with too high spin
-- | contamination.
filterByS2 :: Double -> [ExcState] -> [ExcState]
filterByS2 maxContrib eS = filteredStates
  where
    -- Expectation value of a state for the spin operator
    s x = (fromIntegral (x ^. multiplicity) - 1) / 2
    -- Expectation value of the S^2 operator
    filteredStates = filter
      (\x -> spinContaminationFractionOfHigherState
        (s x) (fromMaybe (idealS2fromMult (x ^. multiplicity)) (x ^. s2)) <= maxContrib
      ) eS

-- | Calculates ideal (pure) S^2 expectation value from a given multiplicity
idealS2fromMult :: Int -> Double
idealS2fromMult mult = s * (s + 1)
  where
    s = (fromIntegral mult - 1) / 2

-- | Convert from Angstrom to Bohr
angstrom2Bohr :: Floating a => a -> a
angstrom2Bohr x = 1.88972613 * x

-- | Convert from Bohr to Angstrom
bohr2Angstrom :: Floating a => a -> a
bohr2Angstrom x = 0.52917721 * x

-- | Given excited states, renumber them based on their energy (this might be helpful if s2filter
-- | removed serveral states and now states are missing).
renumberExcitedStatesEnergy :: [ExcState] -> [ExcState]
renumberExcitedStatesEnergy es = zipWith (\i s -> s & nState .~ i) [1 .. ] . sortOn (^. relEnergy) $ es
