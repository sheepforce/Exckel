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
  { nocalccubes    :: Bool
  , norenderimages :: Bool
  , outdir         :: FilePath
  , vmd            :: Maybe FilePath
  , vmdState       :: Maybe FilePath
  , vmdStartUp     :: Maybe FilePath
  , vmdTemplate    :: FilePath
  , multiwfn       :: Maybe FilePath
  , tachyon        :: Maybe FilePath
  , docx           :: Bool
  , odt            :: Bool
  , docxoutref     :: Maybe FilePath
  , wf             :: Maybe FilePath
  , exc            :: Maybe FilePath
  , imgres         :: (Int, Int)
  , s2Filter       :: Maybe Double
  , foscFilter     :: Maybe Double
  } deriving (Show, Data, Typeable)

exckelArgs = ExckelArgs
  { nocalccubes    =  False
                   &= help "Do not calculate cubes for orbitals and CDDs."
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

  , vmdTemplate    =  (unsafePerformIO $ getDataFileName "VMD.tcl")
                   &= help "VMD template script for plotting."
                   &= typFile

  , multiwfn       =  (unsafePerformIO $ findExecutable "Multiwfn")
                   &= help "Multiwfn executable. Default is first Multiwfn executable found on system"
                   &= typFile

  , tachyon        =  (unsafePerformIO $ findExecutable "tachyon")
                   &= help "Tachyon executable. Default is first tachyon executable found on system"
                   &= typFile

  , docx           =  True
                   &= help "Write output as docx."
                   &= typ "BOOL"

  , odt            =  False
                   &= help "Write output as odt."
                   &= typ "BOOL"

  , docxoutref     =  Nothing
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
  }
