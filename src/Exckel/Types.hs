{-
Defining the data types, that will be used in Exckel. Note that we will follow chemical convention
and call the ground state in given multiplicity X0, the first excited state 1 and so on. Excited
states are formulated in terms of Configuration Interaction and data types represent this. Also
orbital counting starts from 1, no matter which program is being used.
-}
{-# LANGUAGE TemplateHaskell #-}
module Exckel.Types
( Default(..)
, Spin(..)
, OrbitalExcitation(..)
, fromOrb
, toOrb
, CIDeterminant(..)
, excitationPairs
, weight
, WFType(..)
, ExcState(..)
, nState
, multiplicity
, wfType
, s2
, relEnergy
, oscillatorStrength
, ciWavefunction
, nBasisFunctions
, OrbGenerator(..)
, ogExePath
, CDDGenerator(..)
, cddExePath
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
, ImageFiles(..)
, orbImages
, cddImages
, electronImages
, holeImages
, CalcSoftware(..)
, PDDocType(..)
, PandocInfo(..)
, pdRefDoc
, pdDocType
, SpectrumPlotter(..)
, spExePath
, spERange
, spBroadening
, FileInfo(..)
, logFile
, calcSoftware
, waveFunctionFile
, orbGenerator
, cddGenerator
, cubePlotter
, outputPrefix
, cubeFiles
, imageFiles
, imConvertExePath
, pandocInfo
, spectrumPlotter
, selStates
, Cube(..)
, comment
, volumeOrigin
, voxelDimension
, volumeVectorA
, volumeVectorB
, volumeVectorC
, atoms
, volumetricData
, Atom(..)
, atomicNumber
, coordinate
) where
import           Data.Array.Repa       (Array, DIM3, U, (:.))
import qualified Data.Array.Repa       as R
import qualified Data.ByteString.Char8 as B
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Vector
import           Exckel.EmbedContents
import           Lens.Micro.Platform
import           System.Directory
import           System.IO
import           System.IO.Unsafe

class (Default a) where
  def :: a

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
  , _weight          :: Double
  } deriving (Eq, Show)
makeLenses ''CIDeterminant

data WFType = ClosedShell | OpenShell | RestrictedOpenShell deriving (Show, Eq)

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
  , _multiplicity       :: Int
  , _wfType             :: Maybe WFType
  , _s2                 :: Maybe Double
  , _relEnergy          :: Double
  , _oscillatorStrength :: Double
  , _ciWavefunction     :: Vector CIDeterminant
  , _nBasisFunctions    :: Int
  } deriving (Show)
makeLenses ''ExcState

----------------------------------------------------------------------------------------------------
-- File system data type for interaction with external world
----------------------------------------------------------------------------------------------------
-- | Programm to calculate orbital cubes from fchk or molden files
data OrbGenerator =
    MultiWFNOrb
      { _ogExePath          :: FilePath
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
makeLenses ''OrbGenerator
instance (Default OrbGenerator) where
  def = MultiWFNOrb
    { _ogExePath = fromMaybe "Multiwfn" $ unsafePerformIO $ findExecutable "Multiwfn"
    }

-- | Programm to calculate CDD cube files from orbitals
data CDDGenerator =
    MultiWFNCDD
      { _cddExePath :: FilePath
      }
  | REPA
  deriving (Eq, Show)
makeLenses ''CDDGenerator
instance (Default CDDGenerator) where
  def = REPA

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
instance (Default Renderer) where
  def = Tachyon
    { _rExePath = fromMaybe "tachyon" $ unsafePerformIO $ findExecutable "tachyon"
    , _rResolution = (2000,1200)
    , _rImageFormat = PNG
    }

-- | Programm to plot a set of cube files
data CubePlotter =
    VMD
      { _cpExePath   :: FilePath
      , _cpStateFile :: Maybe FilePath
      , _cpTemplate  :: T.Text
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
instance (Default CubePlotter) where
  def = VMD
    { _cpExePath = "vmd"
    , _cpStateFile = Nothing
    , _cpTemplate = T.pack . B.unpack $ vmdTemplateScript
    , _cpRenderer = def
    , _cpStartUp = Nothing
    }

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
instance (Default CubeFiles) where
  def = CubeFiles
    { _orbCubes = Nothing
    , _cddCubes = Nothing
    , _electronCubes = Nothing
    , _holeCubes = Nothing
    }

-- | If images are already available, store all the filepaths here. Naming conventions of other
-- | functions still apply. See CubeFiles data type for more information. Depending on the chosen
-- | ImageFormat the suffix might change.
data ImageFiles = ImageFiles
  { _orbImages      :: Maybe [(Int, FilePath)]
  , _cddImages      :: Maybe [(Int, FilePath)]
  , _electronImages :: Maybe [(Int, FilePath)]
  , _holeImages     :: Maybe [(Int, FilePath)]
  } deriving (Eq, Show)
makeLenses ''ImageFiles
instance (Default ImageFiles) where
  def = ImageFiles
    { _orbImages = Nothing
    , _cddImages = Nothing
    , _electronImages = Nothing
    , _holeImages = Nothing
    }

-- | Quantum chemistry program from which the logfile comes
data CalcSoftware = Gaussian | NWChem deriving (Eq, Show)

-- | Supported output formats for the excitation summary
data PDDocType = DOCX | ODT | LATEX deriving (Eq, Show)

-- | Relevant paths for the pandoc library to create an output document
data PandocInfo = PandocInfo
  { _pdRefDoc  :: Maybe FilePath
  , _pdDocType :: PDDocType
  } deriving (Eq, Show)
makeLenses ''PandocInfo
instance (Default PandocInfo) where
  def = PandocInfo
    { _pdRefDoc = Nothing
    , _pdDocType = DOCX
    }

-- | Programm for plotting spectra and parameters for it
data SpectrumPlotter =
  Gnuplot
    { _spExePath    :: FilePath
    , _spERange     :: Maybe (Double, Double)
    , _spBroadening :: Double
    } deriving (Eq, Show)
makeLenses ''SpectrumPlotter
instance (Default SpectrumPlotter) where
  def = Gnuplot
    { _spExePath = "gnuplot"
    , _spERange = Nothing
    , _spBroadening = 0.3
    }

-- | FilePaths to files, given in absolute paths! Shall be expanded to absolute paths if only
-- | specified as relative path during program execution.
data FileInfo = FileInfo
  { _logFile          :: FilePath
  , _calcSoftware     :: CalcSoftware
  , _waveFunctionFile :: FilePath
  , _orbGenerator     :: OrbGenerator
  , _cddGenerator     :: CDDGenerator
  , _cubePlotter      :: CubePlotter
  , _outputPrefix     :: FilePath
  , _cubeFiles        :: CubeFiles
  , _imageFiles       :: ImageFiles
  , _imConvertExePath :: FilePath
  , _pandocInfo       :: PandocInfo
  , _spectrumPlotter  :: SpectrumPlotter
  , _selStates        :: Maybe [Int]
  }
  deriving (Eq, Show)
makeLenses ''FileInfo
instance (Default FileInfo) where
  def = FileInfo
    { _logFile = ""
    , _calcSoftware = Gaussian
    , _waveFunctionFile = ""
    , _orbGenerator = def
    , _cddGenerator = def
    , _cubePlotter = def
    , _outputPrefix = "."
    , _cubeFiles = def
    , _imageFiles = def
    , _imConvertExePath = fromMaybe "convert" $ unsafePerformIO $ findExecutable "convert"
    , _pandocInfo = def
    , _spectrumPlotter = def
    , _selStates = Nothing
    }

-- | Atoms stored in a gaussian cube file. Atomic coordinates are assumed to be allways in Bohr.
data Atom = Atom
  { _atomicNumber :: Int
  , _coordinate   :: (Double, Double, Double)
  }
  deriving (Eq, Show)
makeLenses ''Atom

-- | A gaussian cube file stored in an array. Units are assumed to be allways in Bohr and the parser
-- | takes care of this.
data Cube = Cube
  { _comment        :: String
  , _volumeOrigin   :: (Double, Double, Double)
  , _voxelDimension :: (Int, Int, Int)
  , _volumeVectorA  :: (Double, Double, Double)
  , _volumeVectorB  :: (Double, Double, Double)
  , _volumeVectorC  :: (Double, Double, Double)
  , _atoms          :: [Atom]
  , _volumetricData :: Array U DIM3 Double
  }
  deriving (Eq, Show)
makeLenses ''Cube
