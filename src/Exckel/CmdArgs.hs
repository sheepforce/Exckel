{-
Data structure for the command line arguments. As many sensible defaults as possible are provided as
possible. Many of these values will be fed into the FileInfo data structure during execution of the
program.
-}

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE NoOverloadedStrings #-}
module Exckel.CmdArgs
( ExckelArgs(..)
, exckelArgs
)
where
import           Paths_Exckel
import           System.Console.CmdArgs
import           System.Directory
import           System.IO
import           System.IO.Unsafe

data ExckelArgs = ExckelArgs
  { nocalcorbs     :: Bool
  , nocalccdds     :: Bool
  , norenderimages :: Bool
  , outdir         :: FilePath
  , vmd            :: Maybe FilePath
  , vmdState       :: Maybe FilePath
  , vmdStartUp     :: Maybe FilePath
  , vmdTemplate    :: Maybe FilePath
  , multiwfn       :: Maybe FilePath
  , cddcalculator  :: String
  , tachyon        :: Maybe FilePath
  , pdFormat       :: String
  , panref         :: Maybe FilePath
  , wf             :: Maybe FilePath
  , exc            :: Maybe FilePath
  , imgres         :: (Int, Int)
  , s2Filter       :: Maybe Double
  , foscFilter     :: Maybe Double
  , fwhm           :: Maybe Double
  , weightfilter   :: Double
  , energyfilter   :: Maybe (Double, Double)
  , states         :: Maybe String
  } deriving (Show, Data, Typeable)

exckelArgs = ExckelArgs
  { nocalcorbs      =  False
                   &= help "Do not calculate cubes for orbitals."
                   &= typ "BOOL"

  , nocalccdds      =  False
                   &= help "Do not calculate charge density difference cubes."
                   &= typ "BOOL"

  , norenderimages =  False
                   &= help "Do not render images from cubes."
                   &= typ "BOOL"

  , outdir         =  (unsafePerformIO getCurrentDirectory)
                   &= help "Destination for all output files and existing cubes."
                   &= typDir

  , vmd            =  (unsafePerformIO $ findExecutable "vmd")
                   &= help "VMD executable. Default is first vmd executable found on system."
                   &= typFile

  , vmdState       =  Nothing
                   &= help "VMD visualisation state file. Used to determine perspective."
                   &= typFile

  , vmdStartUp     =  Nothing
                   &= help "VMD script to set up general look. If none is specified, it will default to your vmdrc."
                   &= typFile

  , vmdTemplate    =  Nothing
                   &= help "VMD template script for plotting."
                   &= typFile

  , multiwfn       =  (unsafePerformIO $ findExecutable "Multiwfn")
                   &= help "Multiwfn executable. Default is first Multiwfn executable found on system"
                   &= typFile

  , cddcalculator  =  "repa"
                   &= help "Program to use to calculate charge density differnces. [multiwfn | repa]"
                   &= typ "STRING"

  , tachyon        =  (unsafePerformIO $ findExecutable "tachyon")
                   &= help "Tachyon executable. Default is first tachyon executable found on system"
                   &= typFile

  , pdFormat       =  "docx"
                   &= help "Format of the summary to write with Pandoc. Any of [docx | odt | latex]"
                   &= typ "STRING"

  , panref         =  Nothing
                   &= help "Reference docx with formatting hints."
                   &= typFile

  , wf             =  Nothing
                   &= help "Wavefunction file (molden or fchk)."
                   &= typFile

  , exc            =  Nothing
                   &= help "Quantum chemistry software output file with excited state informations."
                   &= typFile

  , imgres         =  (2000, 1200)
                   &= help "Image width x heigth for plotting of cubes."
                   &= typ "INT,INT"

  , s2Filter       =  Nothing
                   &= help "Filter excited states by contributions of next higher spin state (applies to plotting and summary)."
                   &= typ "FLOAT"

  , foscFilter     =  Nothing
                   &= help "Filter excited states by minimum oscillator strength (applies only to summary document)."
                   &= typ "FLOAT"

  , fwhm           =  Nothing
                   &= help "Full width at half maximum of the gaussian function used to convolute the stick spectrum."
                   &= typ "FLOAT"

  , weightfilter   =  0.01
                   &= help "Minimum weight of an excitation to write to the summary. (default 0.01)"
                   &= typ "FLOAT"

  , energyfilter   =  Nothing
                   &= help "Energy range (eV) of the excited states of interest and plot range for spectrum."
                   &= typ "FLOAT,FLOAT"

  , states         =  Nothing
                   &= help "Plot specific states and ignore all other criteria in the summary. Give as \"[a,b,c]\""
                   &= typ "[INT]"
  } &= summary "The Exckel automatic summary programm"
    &= help "Available command line arguments. At least \"--wf\" and \"--exc\" must be specified."
