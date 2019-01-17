{-
Defining the data types, that will be used in Exckel. Note that we will follow chemical convention
and call the ground state in given multiplicity X0, the first excited state 1 and so on. Excited
states are formulated in terms of Configuration Interaction and data types represent this. Also
orbital counting starts from 1, no matter which program is being used.
-}

{-# LANGUAGE TemplateHaskell #-}
module Exckel.Types
( Spin(..)
, OrbitalExcitation(..)
, fromOrb
, toOrb
, CIDeterminant(..)
, excitationPairs
, coeff
, ExcState(..)
, nState
, multiplicity
, wfType
, s2
, relEnergy
, oscillatorStrength
, ciWavefunction
) where
import           Data.Vector
import           Lens.Micro.Platform

data Spin = Alpha | Beta deriving (Eq, Show)

-- | Define an excitation pair in a CI determinant
data OrbitalExcitation = OrbitalExcitation
  { _fromOrb :: (Int, Maybe Spin)
  , _toOrb   :: (Int, Maybe Spin)
  } deriving (Eq, Show)
makeLenses ''OrbitalExcitation

-- | A CI determinant is formed by a number of excitations (one for singles determinant, two
-- | for a doubles determinant, ...) and a weight, with which it contributes to the CI wavefunction
data CIDeterminant = CIDeterminant
  { _excitationPairs :: [OrbitalExcitation]
  , _coeff           :: Double
  } deriving (Eq, Show)
makeLenses ''CIDeterminant

data WFType = ClosedShell | OpenShell | RestrictedOpenShell deriving (Show)

-- | From the reference determinant (usually DFT or HF), an excited state is formed by a CI
-- | wavefunction. The state is characterized by its position relative to the ground state
-- |   nState : Position/order relative to the ground state
-- |   multiplicity : Ideal mulitplicity of the state. Together with nState gives something like S5
-- |   wfType : Type of wavefunction that has been used
-- |   s2 :  Expectation value of this state for the S^2 operator
-- |   relEnergy : Energy relative to the ground state in Hartree
-- |   oscillatorStrength : The oscillator strength of the state
-- |   ciWavefunction : the complete CI wavefunction, that forms this state
data ExcState = ExcState
  { _nState             :: Int
  , _multiplicity       :: Maybe Int
  , _wfType             :: Maybe WFType
  , _s2                 :: Maybe Double
  , _relEnergy          :: Double
  , _oscillatorStrength :: Double
  , _ciWavefunction     :: Vector CIDeterminant
  } deriving (Show)
makeLenses ''ExcState
