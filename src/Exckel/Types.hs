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
, WFType(..)
, ExcState(..)
, nState
, multiplicity
, wfType
, s2
, relEnergy
, oscillatorStrength
, ciWavefunction
, CubeGenerator(..)
, cgExePath
, ImageFormat(..)
, Renderer(..)
, rExePath
, rResolution
, rImageFormat
, CubePlotter(..)
, cpExePath
, cpStateFile
, cpTemplate
, cpRenderer
, cpStartUp
, CubeFiles(..)
, orbCubes
, cddCubes
, electronCubes
, holeCubes
, FileInfo(..)
, logFile
, waveFunctionFile
, cubeGenerator
, cubePlotter
, outputPrefix
, cubeFiles
, imConvertExePath
) where
import           Data.Vector
import           Lens.Micro.Platform
import           System.IO

----------------------------------------------------------------------------------------------------
-- Wavefunction data types
----------------------------------------------------------------------------------------------------
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

----------------------------------------------------------------------------------------------------
-- File system data type for interaction with external world
----------------------------------------------------------------------------------------------------
-- | Programm to calculate cubes from fchk or molden files
data CubeGenerator =
    MultiWFN
      { _cgExePath          :: FilePath
      }
{-
  | CubeGen
      { _cgExePath          :: FilePath
      }
  | OrcaPlot
      { _cgExePath          :: FilePath
      , _cgWavefunctionFile :: FilePath
      }
-}
  deriving (Eq, Show)
makeLenses ''CubeGenerator

-- | Image formats, that might be used throughout the program, especially during rendering
data ImageFormat = JPG | PNG deriving (Eq, Show)

-- | Rendering engine for orbital plots
data Renderer =
    Tachyon
      { _rExePath     :: FilePath
      , _rResolution  :: (Int, Int)
      , _rImageFormat :: ImageFormat
      }
  deriving (Eq, Show)
makeLenses ''Renderer

-- | Programm to plot a set of cube files
data CubePlotter =
    VMD
      { _cpExePath   :: FilePath
      , _cpStateFile :: Maybe FilePath
      , _cpTemplate  :: FilePath
      , _cpRenderer  :: Renderer
      , _cpStartUp   :: Maybe FilePath
      }
{-
  | Chimera
      { _cpExePath :: FilePath
      , _cpStateFile :: Maybe FilePath
      }
-}
  deriving (Eq, Show)
makeLenses ''CubePlotter

-- | If cubes are (already) calculated, store the filepaths to all the cubes here. Naming
-- | conventions of other functions still apply. For a given excited state the cubes are called
-- | "electron$X.cube" / "hole$X.cube" / "CDD$X.cube", where "$X" is the number of the excited
-- | state, shown as integer without leading 0 and counting starting from 1. For the orbitals the
-- | naming convention is "orb$X.cube", where $X is the number of the orbital (starting from 1) and
-- | alpha and beta orbitals are only distinguished by their numbers (program dependent)
data CubeFiles = CubeFiles
  { _orbCubes      :: Maybe [FilePath]
  , _cddCubes      :: Maybe [FilePath]
  , _electronCubes :: Maybe [FilePath]
  , _holeCubes     :: Maybe [FilePath]
  } deriving (Eq, Show)
makeLenses ''CubeFiles

-- | FilePaths to files, given in absolute paths! Shall be expanded to absolute paths if only
-- | specified as relative path during program execution.
data FileInfo = FileInfo
  { _logFile          :: Maybe FilePath
  , _waveFunctionFile :: FilePath
  , _cubeGenerator    :: CubeGenerator
  , _cubePlotter      :: CubePlotter
  , _outputPrefix     :: FilePath
  , _cubeFiles        :: CubeFiles
  , _imConvertExePath :: FilePath
  }
  deriving (Eq, Show)
makeLenses ''FileInfo
