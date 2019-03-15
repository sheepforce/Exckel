{-# LANGUAGE OverloadedStrings #-}
module Exckel.CLI.CLI
( initialise
, getExcitedStates
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
import Control.Applicative
import Exckel.Parser hiding (vmdState)
import Data.Attoparsec.Text
import Exckel.ExcUtils

-- | Entry point for the executable. Get command line arguments with defaults and call for check and
-- | from within the check possibly for other routines.
initialise :: ExckelArgs -> IO FileInfo
initialise args = do
  -- Header
  logHeader "\n----"
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
        _         -> TDDFT
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

  -- Command line arguments to populate potential state filtering.
  let ssHigherMultContrib' = s2Filter args
      ssMinimumOscillatorStrenght' = foscFilter args
      ssEnergyFilter' = energyfilter args
      ssSpecificStates' = case (states args) of
        Nothing -> Nothing
        Just s -> (readMaybe :: String -> Maybe [Int]) s
      ssWeightFilter' = weightfilter args
      stateSelection' = StateSelection
        { _ssHigherMultContrib         = ssHigherMultContrib'
        , _ssMinimumOscillatorStrenght = ssMinimumOscillatorStrenght'
        , _ssEnergyFilter              = ssEnergyFilter'
        , _ssSpecificStates            = ssSpecificStates'
        , _ssWeightFilter              = ssWeightFilter'
        }

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
        , _stateSelection   = stateSelection'
        }
  logMessage "Excited state log file"                     (infitialFileInfo ^. logFile)
  logMessage "waveFunctionFile file"                      (infitialFileInfo ^. waveFunctionFile)
  logMessage "QC calculation"                             (show $ infitialFileInfo ^. calcSoftware)
  logMessage "Output directory and search path for files" (infitialFileInfo ^. outputPrefix)
  return infitialFileInfo



-- | Reading the log file, parsing the excited states and filtering of excited states. Returns two
-- | lists of excited states. The first one for obtaining the spectrum, the second one for analysis.
getExcitedStates :: FileInfo -> IO ([ExcState], [ExcState])
getExcitedStates fi = do
  -- Header of section
  logHeader "\n----"
  logHeader "Parsing and filtering:"

  print $ fi ^. calcSoftware

  -- Read and parse the log file.
  logFile <- T.readFile (fi ^. logFile)
  let -- Parse the spectrum
      excitedStatesParse = case (fi ^. calcSoftware) of
        Gaussian
          { _calcType = TDDFT
              { _fullTDDFT = True
              }
          } -> parseOnly gaussianLogTDDFT logFile
        NWChem
          { _calcType = TDDFT
              { _fullTDDFT = True
              }
          } -> parseOnly nwchemTDDFT logFile
        MRCC
          { _calcType = ADC
              { _order   = _
              , _redCost = _
              }
          } -> parseOnly mrccADC logFile
  excitedStatesAll <- case excitedStatesParse of
    Left err        -> do
      errMessage $ "Parsing of the log file failed with: " ++ err
      errMessage "Parsing of log file is required."
      error "Could not parse log file."
    Right excStates -> return excStates

  -- Filter the excited states by multiple criteria
  let -- Filter by maximum contribution of the next higher multiplicity in an open shell calculation.
      excitedStatesByS2 = case (fi ^. stateSelection . ssHigherMultContrib) of
        Nothing   -> excitedStatesAll
        Just maxC -> filterByS2 maxC excitedStatesAll
      -- Filter remaining states by fitting within an energy window.
      excitedStatesByEnergy = case (fi ^. stateSelection . ssEnergyFilter) of
        Nothing           -> excitedStatesByS2
        Just (eMin, eMax) -> filter
          (\x -> (hartree2eV $ x ^. relEnergy) >= eMin && (hartree2eV $ x ^. relEnergy) <= eMax)
          excitedStatesByS2
      -- Filter remaining states by minimum oscillator strength
      excitedStatesByFOsc = case (fi ^. stateSelection . ssMinimumOscillatorStrenght) of
        Nothing      -> excitedStatesByEnergy
        Just minFOsc -> filter (\x -> (x ^. oscillatorStrength) >= minFOsc) excitedStatesByEnergy
      -- If the user has specified to keep only selected states, regardless of what else has been
      -- specified, keep only these states for Cube plotting and so on. If not user specified states
      -- are selected, use the chain of filters resulting in excitedStatesByFOsc.
      excitedStatesFinalFilter = case (fi ^. stateSelection . ssSpecificStates) of
        Nothing -> excitedStatesByFOsc
        Just sStates -> filter (\x -> (x ^. nState) `elem` sStates) excitedStatesAll

  -- Informations about filtering and parsing process.
  case (fi ^. stateSelection . ssSpecificStates) of
    Nothing -> return ()
    Just s -> logMessage "States for analysis (but not plotting)" (show s)
  logMessage "States in log file"                               (show $ length excitedStatesAll)
  logMessage "States removed due to <S**2> deviation"           (show $ length excitedStatesAll - length excitedStatesByS2)
  logMessage "States removed due to energy range"               (show $ length excitedStatesByS2 - length excitedStatesByEnergy)
  logMessage "States removed due to oscillator strength cutoff" (show $ length excitedStatesByEnergy - length excitedStatesByFOsc)
  logMessage "Remaining states for plotting the spectrum"       (show . map (^. nState) $ excitedStatesByS2)
  logMessage "Remaining states for analysis"                    (show . map (^. nState) $ excitedStatesFinalFilter)

  -- Check if anything remains after filtering
  if (length excitedStatesFinalFilter <= 0)
    then do
      errMessage "No excited states left to process. Will exit here"
      error "No excited states left for analysis."
    else return ()

  return (excitedStatesByS2, excitedStatesFinalFilter)
