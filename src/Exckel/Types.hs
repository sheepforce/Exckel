{-
Defining the data types, that will be used in Exckel. Note that we will follow chemical convention
and call the ground state in given multiplicity X0, the first excited state 1 and so on. Excited
states are formulated in terms of Configuration Interaction and data types represent this.
-}

{-# LANGUAGE TemplateHaskell #-}
module Exckel.Types
( OrbitalExcitation(..)
, fromOrb
, toOrb
, CIDeterminant(..)
, excitationPairs
, weight
, ExcState(..)
, nState
, multiplicity
, refWF
, relEnergy
, oscillatorStrength
, ciWavefunction
) where
import           Data.Vector
import           Lens.Micro.Platform

-- | Define an excitation pair in a CI determinant
data OrbitalExcitation = OrbitalExcitation
  { _fromOrb :: Int
  , _toOrb   :: Int
  } deriving (Eq)
makeLenses ''OrbitalExcitation

-- | A CI determinant is formed by a number of excitations (one for singles determinant, two
-- | for a doubles determinant, ...) and a weight, with which it contributes to the CI wavefunction
data CIDeterminant = CIDeterminant
  { _excitationPairs :: [OrbitalExcitation]
  , _weight          :: Double
  } deriving Eq
makeLenses ''CIDeterminant

-- | Expectation value of the S**2 operator
type S2 = Double
data RefData = ClosedShell | OpenShell (Maybe S2) | RestrictedOpenShell (Maybe S2)

-- | From the reference determinant (usually DFT or HF), an excited state is formed by a CI
-- | wavefunction. The state is characterized by its position relative to the ground state
-- |   nState : Position/order relative to the ground state
-- |   multiplicity : Ideal mulitplicity of the state. Together with nState gives something like S5
-- |   refWF : Type of wavefunction that has been used and possibly S**2 value of the state
-- |   relEnergy : Energy relative to the ground state in Hartree
-- |   oscillatorStrength : The oscillator strength of the state
-- |   ciWavefunction : the complete CI wavefunction, that forms this state
data ExcState = ExcState
  { _nState             :: Int
  , _multiplicity       :: Int
  , _refWF              :: RefData
  , _relEnergy          :: Double
  , _oscillatorStrength :: Double
  , _ciWavefunction     :: Vector CIDeterminant
  }
makeLenses ''ExcState
