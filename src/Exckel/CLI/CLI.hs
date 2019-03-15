module Exckel.CLI.CLI
( initialise
)
where
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy       as BL
import           Data.Maybe
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Exckel.CLI.SharedFunctions
import           Exckel.CmdArgs
import           Exckel.EmbedContents
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           Text.Read

-- | Entry point for the executable. Get command line arguments with defaults and call for check and
-- | from within the check possibly for other routines.
initialise :: ExckelArgs -> IO FileInfo
initialise args = do
  -- Header
  logHeader "----"
  logHeader "Intialisation:"

  -- Define the log file path.
  logFile' <- case (exc args) of
    Nothing             -> do
      errMessage "You have not specified a log file of your QC calculation, but this is mandatory."
      error "Log file not specified. Cannot continue."
    Just logFilePathRel -> do
      hasLogFile <- doesFileExist logFilePathRel
      if hasLogFile
        then do
          logFilePathAbs <- makeAbsolute logFilePathRel
          return logFilePathAbs
        else do
          errMessage "Log file not found."
          error "Log file not found. Cannot continue."

  -- Define the wavefunction file path.
  waveFunctionFile' <- case (wf args) of
    Nothing                      -> do
      errMessage "You have not specified a wavefunction file of your QC calculation, but this is mandatory."
      error "Wavefunction file not specified. Cannot continue."
    Just waveFunctionFilePathRel -> do
      hasWavefunctionFile <- doesFileExist waveFunctionFilePathRel
      if hasWavefunctionFile
        then do
          waveFunctionFilePathAbs <- makeAbsolute waveFunctionFilePathRel
          return waveFunctionFilePathAbs
        else do
          errMessage "Wavefunction file not found, but wavefunction file is necessary."
          error "Wavefunction file not found."

  -- Get the type of the calculation for the parser
  let calcType' = case (calctype args) of
        "rc-adc2" -> ADC
          { _order = 2
          , _redCost = True
          }
        "tddft"   -> TDDFT
          { _fullTDDFT = True
          }
      -- Get QC software for the parser
      calcSoftware' = case (calcsoftware args) of
        "gaussian" -> Gaussian
          { _calcType = calcType'
          }
        "nwchem"   -> NWChem
          { _calcType = calcType'
          }
        "mrcc"     -> MRCC
          { _calcType = calcType'
          }
        _          -> Gaussian
          { _calcType = calcType'
          }

  -- Definition and setup of outputPrefix
  let outputPrefixRelPath = outdir args
  outputPrefix' <- makeAbsolute outputPrefixRelPath
  hasOutDir <- doesDirectoryExist outputPrefix'
  if hasOutDir
    then return ()
    else createDirectory outputPrefix'

  -- Define spectrum plotter
  let -- The energy window (if defined) for the plotting program
      spERange' = energyfilter args
      -- The gaussian FWHM for spectrum plotting
      spBroadening' = fromMaybe 0.3 $ fwhm args
  spectrumPlotter' <- case (spectrum args) of
    "gnuplot"   -> do
      gnuplotExe <- findExecutable "gnuplot"
      case gnuplotExe of
        Just exe -> return Gnuplot
          { _spExePath    = exe
          , _spERange     = spERange'
          , _spBroadening = spBroadening'
          }
        _        -> do
          errMessage "Your requested Gnuplot for spectrum plotting, but the Gnuplot executable could not be found."
          error "Gnuplot not found."
    "spectrify" -> return Spectrify
      { _spERange     = spERange'
      , _spBroadening = spBroadening'
      }
    _           -> return Spectrify
      { _spERange     = spERange'
      , _spBroadening = spBroadening'
      }

  -- Define the cube calculation program for orbitals
  orbGenerator' <- if (nocalcorbs args)
    then return Nothing
    else case (multiwfn args) of
      Nothing  -> do
        errMessage "Requested Multiwfn to calculate orbital cubes, but Multiwfn executable has not been found."
        errMessage "Will skip orbital calculation."
        return Nothing
      Just exe -> return $ Just MultiWFNOrb
          { _ogExePath = exe
          }

  -- Define the CDD calculation program
  cddGenerator' <- if (nocalccdds args)
    then return Nothing
    else case (cddcalculator args) of
      "repa"     -> return $ Just REPA
      "multiwfn" -> case (multiwfn args) of
          Nothing  -> do
            errMessage "Requested Multiwfn to calculate CDD cubes, but Multiwfn executable has not been found."
            errMessage "Will skip CDD calculation."
            return Nothing
          Just exe -> return $ Just MultiWFNCDD
            { _cddExePath = exe
            }
  -- Define the program to plot and render the cube files
  imageMagickConvert <- findExecutable "convert"
  cpTemplate' <- case (vmdTemplate args) of
    Nothing       -> return $ T.pack . BS.unpack $ vmdTemplateScript
    Just templatePath -> do
      template <- T.readFile templatePath
      return template
  cubePlotter' <- if (norenderimages args)
    then return Nothing
    else do
      case (vmd args, tachyon args, imageMagickConvert) of
        (Nothing, _, _)                   -> do
          errMessage "Requested VMD to plot cubes, but VMD executable has not been found."
          errMessage "Will skip cube rendering."
          return Nothing
        (_, Nothing, _)                   -> do
          errMessage "Requested Tachyon to render images, but Tachyon executable has not been found."
          errMessage "Will skip cube rendering."
          return Nothing
        (_, _, Nothing)                   -> do
          errMessage "Tachyon nees ImageMagick's convert, but it could not be found."
          errMessage "Will skip cube rendering."
          return Nothing
        (Just vmdExe, Just tachyonExe, Just rIMExePath) -> do
          return $ Just VMD
            { _cpExePath   = vmdExe
            , _cpStateFile = vmdState args
            , _cpTemplate  = cpTemplate'
            , _cpRenderer  = Tachyon
                { _rExePath     = tachyonExe
                , _rResolution  = imgres args
                , _rImageFormat = PNG
                , _rIMExePath   = rIMExePath
                }
            , _cpStartUp   = vmdStartUp args
            }

  -- Look for already existing cube files.
  cubeFiles' <- findAllCubes outputPrefix'

  -- Look for already existing image files.
  imageFiles' <- findAllImages outputPrefix'

  -- Setup the Pandoc informations
  let pdDocType' = case (panFormat args) of
        "docx"  -> DOCX
        "odt"   -> ODT
        "latex" -> LATEX
        _       -> DOCX
  pdRefDoc' <- case (panref args) of
    Nothing -> do
      let panrefFile = outputPrefix' ++ [pathSeparator] ++ "panref.tmp"
      case pdDocType' of
        DOCX -> do
          BS.writeFile panrefFile defaultDocx
          return $ Just panrefFile
        ODT  -> do
          BS.writeFile panrefFile defaultODT
          return $ Just panrefFile
        _    -> return Nothing
  let pandocInfo' = PandocInfo
        { _pdRefDoc  = pdRefDoc'
        , _pdDocType = pdDocType'
        }

  -- Are some states selectively specified?
  let selStates' = case (states args) of
        Nothing -> Nothing
        Just s  -> (readMaybe :: String -> Maybe [Int]) s

  -- The initial information about program execution and files built from gathered values
  let infitialFileInfo = FileInfo
        { _logFile          = logFile'
        , _waveFunctionFile = waveFunctionFile'
        , _outputPrefix     = outputPrefix'
        , _calcSoftware     = calcSoftware'
        , _spectrumPlotter  = spectrumPlotter'
        , _orbGenerator     = orbGenerator'
        , _cddGenerator     = cddGenerator'
        , _cubePlotter      = cubePlotter'
        , _cubeFiles        = cubeFiles'
        , _imageFiles       = imageFiles'
        , _pandocInfo       = pandocInfo'
        , _selStates        = selStates'
        }
  logMessage "Excited state log file"                     $ infitialFileInfo ^. logFile
  logMessage "waveFunctionFile file"                      $ infitialFileInfo ^. waveFunctionFile
  logMessage "QC calculation"                             $ show $ infitialFileInfo ^. calcSoftware
  logMessage "Output directory and search path for files" $ infitialFileInfo ^. outputPrefix
  return infitialFileInfo
