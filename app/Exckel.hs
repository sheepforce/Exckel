{-
This is the app for Exckel. The command line interface, interacting with the user and calling
everything else. There are some assumptions made here.
  - ImageMagick is available (if not -> quit).
  - Gnuplot is available and has the PNG terminal (if gnuplot not available -> quit).
  - Filesystem stays consistent during run. No one is renaiming or deleting files during execution.
-}
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text           hiding (take)
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy           as BL
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import qualified Data.Vector                    as V
import           Exckel.CmdArgs
import           Exckel.CubeGenerator.MultiWFN  as CG.MWFN
import           Exckel.CubePlotter.VMD         as CP.VMD
import           Exckel.DocumentCreator
import           Exckel.EmbedContents
import           Exckel.ExcUtils
import           Exckel.Parser                  hiding (vmdState)
import qualified Exckel.SpectrumPlotter.Gnuplot as SP.GP
import           Exckel.Types
import           Lens.Micro.Platform
import           Paths_Exckel
import           System.Console.ANSI
import           System.Console.CmdArgs         hiding (def)
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Pandoc                    hiding (FileInfo, def,
                                                 getDataFileName)
import qualified Text.Pandoc                    as PD (def)
import           Text.Printf

logMessage f s = printf "  %-70s : %-30s\n" f s
logHeader h = do
  setSGR [SetColor Foreground Vivid Green]
  hPutStrLn stdout h
  setSGR [Reset]
logInfo i = do
  setSGR [SetColor Foreground Vivid Blue]
  hPutStrLn stdout i
  setSGR [Reset]
errMessage x = do
  setSGR [SetColor Foreground Vivid Red]
  hPutStrLn stdout x
  setSGR [Reset]


-- | Entry point for the executable. Get command line arguments with defaults and call for check and
-- | from within the check possibly for other routines.
main :: IO ()
main = do
  -- print logo
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ BS.unpack exckelLogo
  setSGR [Reset]
  -- read command line arguments and defaults for them
  arguments <- cmdArgs exckelArgs
  -- look for ImageMagicks executable on the system
  imageMagick <- findExecutable "convert"
  -- look for gnuplot executable on the system
  gnuplot <- findExecutable "gnuplot"
  -- make sure output directory exists
  hasOutDir <- doesDirectoryExist (outdir arguments)
  case hasOutDir of
    True  -> return ()
    False -> createDirectory (outdir arguments)
  case (imageMagick, gnuplot) of
    -- if it is not present, dont continue
    (Nothing, _) -> errMessage "Could not find imageMagick's \"convert\". Will abbort here."
    (_, Nothing) -> errMessage "Could not find gnuplot. Will abort here."
    -- if found, call checker for minimum input
    (Just convert, Just gnuplot) -> checkInitial (convert, gnuplot) arguments


-- | Initial check if enough informations are provided and if everything requested makes sense. If
-- | not print an error and exit. If yes, prepare FileInfo for further use and go for next step.
-- | Expansion to absolute paths happening here for some records.
-- |   imC -> path to ImageMagick's convert
-- |   gnp -> path to Gnuplot
-- |   a -> ExckelArgs data structure coming from the initial call
checkInitial :: (FilePath, FilePath) -> ExckelArgs -> IO ()
checkInitial (imC, gnp) a = do
  logHeader "----"
  logHeader "Initial file checks:"
  case (wf a, exc a) of
    -- both wavefunction file and logfile are defined
    (Just w, Just l) -> do
      excAP <- makeAbsolute l
      wfAP <- makeAbsolute w
      outPrefAP <- makeAbsolute (outdir a)
      let fileInfo = def
            & logFile .~ excAP
            & waveFunctionFile .~ wfAP
            & outputPrefix .~ outPrefAP
            & imConvertExePath .~ imC
            & spectrumPlotter . spExePath .~ gnp
            & spectrumPlotter . spBroadening .~ fromMaybe 0.3 (fwhm a)
      logMessage "QC log file with excited states" (fileInfo ^. logFile)
      logMessage "Wavefunction file" (fileInfo ^. waveFunctionFile)
      logMessage "Work directoring" (fileInfo ^. outputPrefix)
      logMessage "ImageMagick executable" (fileInfo ^. imConvertExePath)
      logMessage "Plotting software executable" (fileInfo ^. spectrumPlotter . spExePath)
      logMessage "Calculation software was" (show (fileInfo ^. calcSoftware))
      getExcitedStates a fileInfo
    -- one or both of wavefunction and/or logfile are undefined
    _ -> errMessage $ "Wavefuntion and or log file not found. Cannot continue."

-- | Get the excited states from the log file and pass to next step.
getExcitedStates :: ExckelArgs -> FileInfo -> IO ()
getExcitedStates a fi = do
  logHeader "\n----"
  logHeader "Processing log file and filter excited states:"
  logMessage
    "Maximum contribution of higher multiplicities allowed (by <S**2>)"
    ( case (s2Filter a) of
        Nothing      -> "/"
        Just contrib -> show contrib
    )
  logMessage
    "Minimum oscillator strength"
    ( case (foscFilter a) of
       Nothing   -> "/"
       Just minF -> show minF
    )
  logInfo "Parsing log file ..."
  logRaw <- T.readFile (fi ^. logFile)
  let logParsed = case (fi ^. calcSoftware) of
        -- if guassian, use the gaussian parser
        Gaussian -> parseOnly gaussianLogTDDFT logRaw
  case logParsed of
    -- if parsing went wrong show the error
    Left err -> do
      setSGR [SetColor Foreground Vivid Red]
      errMessage $ "  Could not parse the log file with: " ++ show err
      setSGR [Reset]
    -- if parsing suceeded, pass data to next step
    Right eS -> do
      logMessage "Reference wavefunction" (show . head . map (^. wfType) $ eS)
      logMessage "Ground state multiplicity" (show . head . map (^. multiplicity) $ eS)
      logMessage "Number of excited states" (show . length $ eS)
      -- here you could potentially filter the excited states
      let eSfilterByS2 = case (s2Filter a) of
            Nothing      -> eS
            Just contrib -> filterByS2 contrib eS
          eSfilterByFOsc = case (foscFilter a) of
            Nothing -> eSfilterByS2
            Just strength -> filter (\x -> (x ^. oscillatorStrength) >= strength) eSfilterByS2
      logMessage "Number of removed states due to <S**2> deviation" (show $ length eS - length eSfilterByS2)
      logMessage "Number of removed states due to oscillator strength cutoff" (show $ length eSfilterByS2 - length eSfilterByFOsc)
      logMessage "States remaining" (show . map (^. nState) $ eSfilterByFOsc)
      if (length eSfilterByFOsc <= 0)
        then errMessage "No states left to plot. Will exit here"
        else do
          logInfo $ "Plotting spectrum as Spectrum.png. See Gnuplot.out and Gnuplot.err"
          SP.GP.plotSpectrum fi eSfilterByS2 eSfilterByFOsc
          doCubes a fi eSfilterByFOsc

-- | Routine to calculate the cubes. Wraps the CubeGenerators. Jumps to next step if no cubes are to
-- | be calculated.
doCubes :: ExckelArgs -> FileInfo -> [ExcState]-> IO ()
doCubes a fi eS = do
  logHeader "\n----"
  logHeader "Calculating cube data:"
  let -- filter the CI determinants of the excited states by weights of excitations, so
      -- that only excitations with a high weight remain
      weightFilteredCIDeterminants =
        map (V.filter (\d -> d ^. weight >= weightfilter a)) $
        map (^. ciWavefunction) eS
      weightFilteredExcStates = zipWith (\e d -> e & ciWavefunction .~ d) eS weightFilteredCIDeterminants
  logMessage "Filtering CI determinants by minimum weights" (show $ weightfilter a)
  logMessage
    "Calculate cubes"
    ( if (nocalccubes a)
       then "no"
       else "yes"
    )
  if (nocalccubes a)
    -- cubes shall not be calculated
    then return ()
    -- cubes shall be calculated
    else do
      case fi ^. cubeGenerator of
        -- program to calculate cubes from wavefunction is MultiWFN
        MultiWFN{} -> do
          case (multiwfn a) of
            -- MultiWFN executable has not been found
            Nothing -> errMessage "MultiWFN executable not found. Please specify one. Will skip cube calculation."
            -- MultiWFN executable has been found
            Just exe -> do
              let fileInfo = fi
                    & cubeGenerator . cgExePath .~ exe
              logMessage "CubeCalculator" (fileInfo ^. cubeGenerator . cgExePath)
              logMessage "Orbitals to plot" (show . nub . concat . map getOrbNumbers $ eS)
              logInfo "Calculating orbital cubes. See \"MultiWFN.out\" and \"MultiWFN.err\""
              CG.MWFN.calculateOrbs fileInfo (nub . concat . map getOrbNumbers $ weightFilteredExcStates)
              logInfo "Calculating CDDs. See \"MultiWFN.out\" and \"MultiWFN.err\""
              CG.MWFN.calculateCDDs fileInfo (map (^. nState) eS)
  doPlots a fi weightFilteredExcStates

-- | Call plotter to visualise all cubes found.
doPlots :: ExckelArgs -> FileInfo -> [ExcState] -> IO ()
doPlots a fi eS = do
  logHeader "\n----"
  logHeader "Plotting cubes:"
  logMessage
    "Plot cubes"
    ( if (norenderimages a)
        then "no"
        else "yes"
    )
  if (norenderimages a)
    -- cubes will not be plotted
    then return ()
    -- cubes will be plotted
    else do
      outDirContents <- listDirectory (fi ^. outputPrefix)
      let allCubes = filter (\x -> (takeExtension x) == ".cube") $ outDirContents
      absoluteCubes <- mapM makeAbsolute $ map ((fi ^. outputPrefix ++ "/") ++) allCubes
      let orbCubesFiles = filter (\x -> (take 3 . takeBaseName $ x) == "orb") absoluteCubes
          cddCubesFiles = filter (\x -> (take 3 . takeBaseName $ x) == "CDD") absoluteCubes
          electronCubesFiles = filter (\x -> (take 8 . takeBaseName $ x) == "electron") absoluteCubes
          holeCubesFiles = filter (\x -> (take 4 . takeBaseName $ x) == "hole") absoluteCubes
          fileInfoWithCubes = fi
            & cubeFiles . orbCubes .~ Just orbCubesFiles
            & cubeFiles . cddCubes .~ Just cddCubesFiles
            & cubeFiles . electronCubes .~ Just electronCubesFiles
            & cubeFiles . holeCubes .~ Just holeCubesFiles
      logMessage "Orbital cubes" (show $ map takeFileName <$> (fileInfoWithCubes ^. cubeFiles . orbCubes))
      logMessage "CDD cubes" (show $ map takeFileName <$> (fileInfoWithCubes ^. cubeFiles . cddCubes))
      logMessage "Electron density cubes" (show $ map takeFileName <$> (fileInfoWithCubes ^. cubeFiles . electronCubes))
      logMessage "Hole density cubes" (show $ map takeFileName <$> (fileInfoWithCubes ^. cubeFiles . holeCubes))
      case fileInfoWithCubes ^. cubePlotter of
        -- VMD is selected as a plotter for cubes
        VMD{} -> do
          case (vmd a, tachyon a) of
            -- the vmd executable has not been found by the intial arguments or defaults
            (Nothing, _) -> errMessage "VMD executable not found. Please specify one. Will skip cube plotting."
            (Just _, Nothing) -> errMessage "Tachyon executable not found. Please specify one. Will skip cube plotting."
            -- vmd has been found
            (Just vmdExe, Just tacExe) -> do
              let fileInfoWithVMDWithoutTemplate = fileInfoWithCubes
                    & cubePlotter . cpExePath .~ vmdExe
                    & cubePlotter . cpStateFile .~ (vmdState a)
                    & cubePlotter . cpStartUp .~ (vmdStartUp a)
                    & cubePlotter . cpRenderer . rExePath .~ tacExe
                    & cubePlotter . cpRenderer . rResolution .~ (imgres a)
                    & cubePlotter . cpRenderer . rImageFormat .~ PNG
              fileInfoWithVMDAndTemplate <- case (vmdTemplate a) of
                    -- no other template than built in default specified
                    Nothing -> return fileInfoWithVMDWithoutTemplate
                    -- filepath to other template specified
                    Just t -> do
                      template <- T.readFile t
                      return $ fileInfoWithVMDWithoutTemplate & cubePlotter . cpTemplate .~ template
              logMessage "Cube plotter" (fileInfoWithVMDAndTemplate ^. cubePlotter . cpExePath)
              logMessage "VMD state file with perspective" (fromMaybe "None (will use default perspective)" $ fileInfoWithVMDAndTemplate ^. cubePlotter . cpStateFile)
              logMessage "VMD start up file with general settings" (fromMaybe "None (will look for $HOME/.vmdrc and use defaults if non existant)" $ fileInfoWithVMDAndTemplate ^. cubePlotter . cpStartUp)
              logMessage "VMD template script" (case (vmdTemplate a) of
                Nothing -> "None (using built in default)"
                Just t  -> t
                )
              logMessage "Rendering engine" (fileInfoWithVMDAndTemplate ^. cubePlotter . cpRenderer . rExePath)
              logMessage "Rendering resolution" (show $ fileInfoWithVMDAndTemplate ^. cubePlotter . cpRenderer . rResolution)
              logMessage "Renderer image format" (show $ fileInfoWithVMDAndTemplate ^. cubePlotter . cpRenderer . rImageFormat)
              logInfo "Calling VMD and Tachyon now. See \"VMD.out\", \"VMD.err\", \"Tachyon.out\" and \"Tachyon.err\""
              CP.VMD.plotCubes fileInfoWithVMDAndTemplate
  doSummaryDocument a fi eS

-- | Use Pandoc to create the summary document, using all pictures that are there by now.
doSummaryDocument :: ExckelArgs -> FileInfo -> [ExcState] -> IO ()
doSummaryDocument a fi eS = do
  logHeader "\n----"
  logHeader "Creating summary document:"
  outDirContents <- listDirectory (fi ^. outputPrefix)
  allImageFiles <- mapM makeAbsolute $
    map (((fi ^. outputPrefix) ++ [pathSeparator]) ++) .
    filter
      (\x -> (takeExtension x) == "." ++ (map toLower . show $ PNG) ||
             (takeExtension x) == "." ++ (map toLower . show $ JPG)
      ) $ outDirContents
  let orbImageFiles = filter (\x -> (take 3 . takeBaseName $ x) == "orb") allImageFiles
      cddImageFiles = filter (\x -> (take 3 . takeBaseName $ x) == "CDD") allImageFiles
      electronImageFiles = filter (\x -> (take 8 . takeBaseName $ x) == "electron") allImageFiles
      holeImageFiles = filter (\x -> (take 4 . takeBaseName $ x) == "hole") allImageFiles
      --
      orbFileNumbers = map ((read :: String -> Int) . drop 3 . takeBaseName) $ orbImageFiles
      cddFileNumbers = map ((read :: String -> Int) . drop 3 . takeBaseName) $ cddImageFiles
      electronFileNumbers = map ((read :: String -> Int) . drop 8 . takeBaseName) $ electronImageFiles
      holeFileNumbers = map ((read :: String -> Int) . drop 4 . takeBaseName) $ holeImageFiles
      --
      orbImageFilesIndexed = zip orbFileNumbers orbImageFiles
      cddImageFilesIndexed = zip cddFileNumbers cddImageFiles
      electronImageFilesIndexed = zip electronFileNumbers electronImageFiles
      holeImageFilesIndexed = zip holeFileNumbers holeImageFiles
      --
      panFormat = case (pdFormat a) of
        "docx"  -> DOCX
        "odt"   -> ODT
        "latex" -> LATEX
        _       -> DOCX
  --
  absPanRefDoc <- case (panref a, panFormat) of
    (Just ref, _) -> pure <$> makeAbsolute ref
    (Nothing, form) -> do
      let panrefFile = (fi ^. outputPrefix) ++ [pathSeparator] ++ "panref.tmp"
      case form of
        DOCX -> do
          BS.writeFile panrefFile defaultDocx
          return $ Just panrefFile
        ODT -> do
          BS.writeFile panrefFile defaultODT
          return $ Just panrefFile
        _ -> return Nothing
  let fileInfoWithImagesAndPandoc = fi
        & imageFiles . orbImages .~ Just (sortOn fst orbImageFilesIndexed)
        & imageFiles . cddImages .~ Just cddImageFilesIndexed
        & imageFiles . electronImages .~ Just electronImageFilesIndexed
        & imageFiles . holeImages .~ Just holeImageFilesIndexed
        & pandocInfo . pdRefDoc .~ absPanRefDoc
        & pandocInfo . pdDocType .~ panFormat
  logMessage "Pandoc output format for summary" (show $ fileInfoWithImagesAndPandoc ^. pandocInfo . pdDocType)
  logMessage "Pandoc reference document" ((fromMaybe "Not set (default Pandoc formatting) " $ fileInfoWithImagesAndPandoc ^. pandocInfo . pdRefDoc) ++ " (if \"panref.tmp\", this was autogenerated)")
  logMessage "Images for these orbitals available" (show $ map fst <$> fileInfoWithImagesAndPandoc ^. imageFiles . orbImages)
  logMessage "CDDs images for these states available" (show $ map fst <$> fileInfoWithImagesAndPandoc ^. imageFiles . cddImages)
  logMessage "Electron images for these states available" (show $ map fst <$> fileInfoWithImagesAndPandoc ^. imageFiles . electronImages)
  logMessage "Hole images for these states available" (show $ map fst <$> fileInfoWithImagesAndPandoc ^. imageFiles . holeImages)
  --
  let summary = excitationSummary fileInfoWithImagesAndPandoc eS
  case fileInfoWithImagesAndPandoc ^. pandocInfo . pdDocType of
    DOCX -> do
      logMessage "Summary document format" "Microsoft Word Document (docx)"
      let refDocx = fileInfoWithImagesAndPandoc ^. pandocInfo . pdRefDoc
      summaryDoc <- runIO $ do
        writeDocx PD.def {writerReferenceDoc = refDocx} summary
      case summaryDoc of
        Left err -> errMessage $ "Error occured during generation of the pandoc summary: " ++ show err
        Right doc -> do
          logInfo "Writing document to \"summary.docx\""
          BL.writeFile
            ((fileInfoWithImagesAndPandoc ^. outputPrefix) ++ [pathSeparator] ++ "summary.docx") doc
    ODT -> do
      logMessage "Summary document format" "Open Document Text (odt)"
      let refODT = fileInfoWithImagesAndPandoc ^. pandocInfo . pdRefDoc
      summaryDoc <- runIO $ do
        odtTemplate <- getDefaultTemplate "odt"
        writeODT PD.def {writerReferenceDoc = refODT, writerTemplate = Just odtTemplate} summary
      case summaryDoc of
        Left err -> do
          errMessage $ "Error occured during generation of the pandoc summary: " ++ show err
        Right doc -> do
          logInfo "Writing document to \"summary.odt\""
          BL.writeFile
            ((fileInfoWithImagesAndPandoc ^. outputPrefix) ++ [pathSeparator] ++ "summary.odt") doc
    LATEX -> do
      logMessage "Summary document format" "LATEX (tex)"
      summaryDoc <- runIO $ do
        texTemplate <- getDefaultTemplate "latex"
        writeLaTeX PD.def {writerTemplate = Just texTemplate} summary
      case summaryDoc of
        Left err -> errMessage $ "Error occured during generation of the pandoc summary: " ++ show err
        Right doc -> do
          logInfo "Writing document to \"summary.tex\". You can run xelatex on it."
          T.writeFile
            ((fileInfoWithImagesAndPandoc ^. outputPrefix) ++ [pathSeparator] ++ "summary.tex") doc
