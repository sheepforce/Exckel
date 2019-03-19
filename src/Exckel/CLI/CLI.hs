{-# LANGUAGE OverloadedStrings #-}
module Exckel.CLI.CLI
( initialise
, getExcitedStates
, plotSpectrum
, calcOrbCubes
, calcCDDCubes
, doPlots
, createSummaryDocument
)
where
import           Data.Attoparsec.Text
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BL
import           Data.List
import           Data.Maybe
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import qualified Data.Vector                      as V
import           Exckel.CLI.SharedFunctions
import           Exckel.CmdArgs
import qualified Exckel.CubeGenerator.Exckel      as CG.Exckel
import qualified Exckel.CubeGenerator.MultiWFN    as CG.MWFN
import qualified Exckel.CubePlotter.VMD           as CP.VMD
import           Exckel.DocumentCreator
import           Exckel.EmbedContents
import           Exckel.ExcUtils
import           Exckel.Parser                    hiding (vmdState)
import qualified Exckel.SpectrumPlotter.Gnuplot   as SP.GP
import qualified Exckel.SpectrumPlotter.Spectrify as SP.SP
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           Text.Pandoc                      hiding (FileInfo, def,
                                                   getDataFileName)
import qualified Text.Pandoc                      as PD (def)
import           Text.Read

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
      _          -> return $ Just REPA
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
        (Just vmdExe, Just tachyonExe, Just convertExe) -> do
          return $ Just VMD
            { _cpExePath   = vmdExe
            , _cpStateFile = vmdState args
            , _cpTemplate  = cpTemplate'
            , _cpRenderer  = Tachyon
                { _rExePath     = tachyonExe
                , _rResolution  = imgres args
                , _rImageFormat = PNG
                , _rIMExePath   = convertExe
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
    Nothing  -> do
      let panrefFile = outputPrefix' ++ [pathSeparator] ++ "panref.tmp"
      case pdDocType' of
        DOCX -> do
          BS.writeFile panrefFile defaultDocx
          return $ Just panrefFile
        ODT  -> do
          BS.writeFile panrefFile defaultODT
          return $ Just panrefFile
        _    -> return Nothing
    Just ref -> do
      absRefDoc <- makeAbsolute ref
      return $ Just absRefDoc
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
        Just s  -> (readMaybe :: String -> Maybe [Int]) s
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
-- | lists of excited states. The first one for obtaining the spectrum, the second one labeling the
-- | spectrum and the third for analysis in the table.
getExcitedStates :: FileInfo -> IO ([ExcState], [ExcState], [ExcState])
getExcitedStates fi = do
  -- Header of section
  logHeader "\n----"
  logHeader "Parsing and filtering:"

  print $ fi ^. calcSoftware

  -- Read and parse the log file.
  logFile' <- T.readFile (fi ^. logFile)
  let -- Parse the spectrum
      excitedStatesParse = case (fi ^. calcSoftware) of
        Gaussian
          { _calcType = TDDFT
              { _fullTDDFT = True
              }
          } -> parseOnly gaussianLogTDDFT logFile'
        NWChem
          { _calcType = TDDFT
              { _fullTDDFT = True
              }
          } -> parseOnly nwchemTDDFT logFile'
        MRCC
          { _calcType = ADC
              { _order   = _
              , _redCost = _
              }
          } -> parseOnly mrccADC logFile'
        _   -> parseOnly gaussianLogTDDFT logFile'
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
    Just s  -> logMessage "States for analysis (but not plotting)" (show s)
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

  return (excitedStatesByS2, excitedStatesByFOsc, excitedStatesFinalFilter)



-- | Plotting the spectrum from excited states. Takes all excited states, that contribute to the
-- | spectrum as first list of excited states and the excited states, that shall be used for
-- | labeling as a second lsit of excited states.
plotSpectrum :: FileInfo -> ([ExcState], [ExcState]) -> IO ()
plotSpectrum fi (esAll, esLabel) = do
  -- Header
  logHeader "\n----"
  logHeader "Plotting the spectrum:"

  logMessage "Spectrum plotter" $ case (fi ^. spectrumPlotter) of
    Spectrify {} -> "spectrify.py"
    Gnuplot {}   -> show $ fi ^. spectrumPlotter ^. spExePath

  -- Call the plotting software.
  case (fi ^. spectrumPlotter) of
    Spectrify {} -> do
      logInfo "Plotting spectrum as \"Spectrum.png\". See \"Spectrify.out\" and \"Spectrify.err\"."
      SP.SP.plotSpectrum fi esAll esLabel
    Gnuplot {}   -> do
      logInfo "Plotting spectrum as \"Spectrum.png\". See \"Gnuplot.out\" and \"Gnuplot.err\"."
      SP.GP.plotSpectrum fi esAll esLabel



-- | Calculate cubes for the orbitals. Gives back updated file infos with all orbital cubes.
calcOrbCubes :: FileInfo -> [ExcState] -> IO FileInfo
calcOrbCubes fi es = do
  -- Header
  logHeader "\n----"
  logHeader "Orbital calculation:"

  logMessage "Calculate orbital cubes" $ case (fi ^. orbGenerator) of
    Nothing -> "no"
    Just _  -> "yes"

  -- Depending on what is going to happen with the cubes in subsequent steps, we can potentially
  -- reduce the number of orbitals to plot, by first applying the weight filter on the CI
  -- determinants.
  let ciDeterminantsByWeight =
        map (V.filter (\d -> d ^. weight >= (fi ^. stateSelection . ssWeightFilter))) $
        map (^. ciWavefunction) es
      excitedStatesByCIWeight =
        zipWith (\e d -> e & ciWavefunction .~ d) es ciDeterminantsByWeight
  excitedStatesForOrbs <- case (fi ^. cddGenerator) of
        -- Repa will need all grid data to be present
        Just REPA {}        -> return es
        -- Multiwfn will calculate CDD grids by itself from scratch, so orbital numbers can be reduced
        Just MultiWFNCDD {} -> do
          logMessage
            "Filtered CI determinants by minimum weight of an excitation"
            (show $ fi ^. stateSelection . ssWeightFilter)
          return excitedStatesByCIWeight
        Nothing             -> return es
  let orbitalsToPlot = nub . concat . map getOrbNumbers $ excitedStatesForOrbs
  logMessage "Orbitals to calculate" (show orbitalsToPlot)

  -- Do the calculation and update the file info with the newly found cubes after calculation.
  cubeInfo <- case (fi ^. orbGenerator) of
    Nothing          -> do
      return $ fi ^. cubeFiles
    Just MultiWFNOrb {} -> do
      logMessage "Orbital calculator"    (fi ^. orbGenerator . _Just . ogExePath)
      logInfo "Calculating orbitals now. See \"MultiWFN.out\" and \"MultiWFN.err\"."
      CG.MWFN.calculateOrbs fi orbitalsToPlot
      allCubes <- findAllCubes (fi ^. outputPrefix)
      return allCubes

  return $ fi & cubeFiles . orbCubes .~ (cubeInfo ^. orbCubes)



-- | Calculate the CDD cubes (including electrons and holes). Gives back updated file infos with all
-- | cdd cubes.
calcCDDCubes :: FileInfo -> [ExcState] -> IO FileInfo
calcCDDCubes fi es = do
  -- Header
  logHeader "\n----"
  logHeader "Charge density difference calcultion:"

  logMessage "Calculate CDD cubes" $ case (fi ^. cddGenerator) of
    Nothing -> "no"
    Just _  -> "yes"

  cubeInfo <- case (fi ^. cddGenerator) of
    Just REPA {}        -> do
      logMessage "CDD calculator" "REgular Parallel Arrays (neglecting all cross terms!)"
      logMessage "Calculating CDDs for states" (show $ map (^. nState) es)
      logMessage "Orbital cubes available" (show $ map takeFileName <$> (fi ^. cubeFiles . orbCubes))
      logInfo "Calculating CDDs in parallel using REPA. See \"REPA.log\""
      mapM_ (\s -> do
        cddTriple <- CG.Exckel.calculateCDD fi s
        case cddTriple of
          Left e               -> errMessage e
          Right (i, (h, e, d)) -> do
            T.writeFile
              ((fi ^. outputPrefix) ++ [pathSeparator] ++ "hole" ++ show i ++ ".cube")
              (CG.Exckel.writeCube h)
            T.writeFile
              ((fi ^. outputPrefix) ++ [pathSeparator] ++ "electron" ++ show i ++ ".cube")
              (CG.Exckel.writeCube e)
            T.writeFile
              ((fi ^. outputPrefix) ++ [pathSeparator] ++ "CDD" ++ show i ++ ".cube")
              (CG.Exckel.writeCube d)
        ) es
      allCubes <- findAllCubes (fi ^. outputPrefix)
      return allCubes
    Just MultiWFNCDD {} -> do
      logMessage "CDD calculator" (show $ fi ^. cddGenerator . _Just . cddExePath)
      logMessage "Calculating CDDs for states" (show $ map (^. nState) es)
      logInfo "Calculating CDDs. See \"MultiWFN.out\" and \"MultiWFN.err\""
      CG.MWFN.calculateCDDs fi (map (^. nState) es)
      allCubes <- findAllCubes (fi ^. outputPrefix)
      return allCubes
    Nothing             -> return $ fi ^. cubeFiles

  return $ fi
    & cubeFiles . holeCubes     .~ (cubeInfo ^. holeCubes)
    & cubeFiles . electronCubes .~ (cubeInfo ^. electronCubes)
    & cubeFiles . cddCubes      .~ (cubeInfo ^. cddCubes)



-- | Plot and render images based on the cube files.
doPlots :: FileInfo -> IO FileInfo
doPlots fi = do
  -- Header
  logHeader "\n----"
  logHeader "Plotting and rendering cubes:"

  logMessage "Plot and render cubes" $ case (fi ^. cubePlotter) of
    Nothing -> "no"
    Just _  -> "yes"

  imageInfo <- case (fi ^. cubePlotter) of
    Just VMD {} -> do
      -- Informations about the current settings
      logMessage "Orbital cubes to plot" $ show $ fi ^. cubeFiles . orbCubes . _Just
      logMessage "Hole density cubes to plot" $ show $ fi ^. cubeFiles . holeCubes . _Just
      logMessage "Electron density cubes to plot" $ show $ fi ^. cubeFiles . electronCubes . _Just
      logMessage "CDD cubes to plot" $ show $ fi ^. cubeFiles . cddCubes . _Just
      logMessage "Cube plotter" $ fi ^. cubePlotter . _Just . cpExePath
      logMessage "Cube renderer" $ fi ^. cubePlotter . _Just . cpRenderer . rExePath
      logMessage "VMD startup file with general settings" $ fromMaybe
        "/ (will try to use your \".vmdrc\")"
        (fi ^. cubePlotter . _Just . cpStartUp)
      logMessage "VMD state file for perspective" $ fromMaybe
        "/ (will use default perspective)"
        (fi ^. cubePlotter . _Just . cpStateFile)
      let unwrappedRenderer = fromJust $ fi ^. cubePlotter
      logMessage "Rendering resolution" $ show (unwrappedRenderer ^. cpRenderer . rResolution)
      logMessage "Rendering image format" $ show (unwrappedRenderer ^. cpRenderer . rImageFormat)
      logInfo "Calling VMD and Tachyon. See \"VMD.out\", \"VMD.err\", \"Tachyon.out\" and \"Tachyon.err\""
      CP.VMD.plotCubes fi
      allImages <- findAllImages (fi ^. outputPrefix)
      return allImages
    Nothing     -> return $ fi ^. imageFiles

  return (fi & imageFiles .~ imageInfo)



-- | Use all informations available to create a summary of the calculation with Pandoc.
createSummaryDocument :: FileInfo -> [ExcState] -> IO ()
createSummaryDocument fi es = do
  -- Header
  logHeader "\n----"
  logHeader "Creating summary document:"

  logMessage "Pandoc output format" $ show (fi ^. pandocInfo . pdDocType)
  logMessage "Pandoc formatting reference document" $ show (fi ^. pandocInfo . pdRefDoc) ++ ("if \"panref.tmp\" this is the builtin default")
  logMessage "Orbital images available" $ show (fi ^. imageFiles . orbImages . _Just)
  logMessage "Hole images available" $ show (fi ^. imageFiles . holeImages . _Just)
  logMessage "Electron images available" $ show (fi ^. imageFiles . electronImages . _Just)
  logMessage "CDD images available" $ show (fi ^. imageFiles . cddImages . _Just)

  let summary = excitationSummary fi es
  case (fi ^. pandocInfo . pdDocType) of
    DOCX  -> do
      summaryDoc <- runIO $ do
        writeDocx PD.def {writerReferenceDoc = fi ^. pandocInfo . pdRefDoc} summary
      case summaryDoc of
        Left err  -> errMessage $ "Error occured during generation of the pandoc summary:" ++ show err
        Right doc -> do
          logInfo "Writing document to \"summary.docx\"."
          BL.writeFile ((fi ^. outputPrefix) ++ [pathSeparator] ++ "summary.docx") doc
    ODT   -> do
      summaryDoc <- runIO $ do
        odtTemplate <- getDefaultTemplate "odt"
        writeODT PD.def {writerReferenceDoc = fi ^. pandocInfo . pdRefDoc, writerTemplate = Just odtTemplate} summary
      case summaryDoc of
        Left err  -> errMessage $ "Error occured during generation of the pandoc summary:" ++ show err
        Right doc -> do
          logInfo "Writing document to \"summary.odt\"."
          BL.writeFile ((fi ^. outputPrefix) ++ [pathSeparator] ++ "summary.odt") doc
    LATEX -> do
      summaryDoc <- runIO $ do
        texTemplate <- getDefaultTemplate "latex"
        writeLaTeX PD.def {writerTemplate = Just texTemplate} summary
      case summaryDoc of
        Left err  -> errMessage $ "Error occured during generation of the pandoc summary:" ++ show err
        Right doc -> do
          logInfo "Writing document to \"summary.tex\". You may compile this using xelatex."
          T.writeFile ((fi ^. outputPrefix) ++ [pathSeparator] ++ "summary.tex") doc