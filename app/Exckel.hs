import           Data.Attoparsec.Text          hiding (take)
import           Data.Char
import           Data.List
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Exckel.CmdArgs
import           Exckel.CubeGenerator.MultiWFN as CG.MWFN
import           Exckel.CubePlotter.VMD        as CP.VMD
import           Exckel.DocumentCreator
import           Exckel.ExcUtils
import           Exckel.Parser                 hiding (vmdState)
import           Exckel.Types
import           Lens.Micro.Platform
import           Paths_Exckel
import           System.Console.CmdArgs        hiding (def)
import           System.Directory
import           System.FilePath

-- | Entry point for the executable. Get command line arguments with defaults and call for check and
-- | from within the check possibly for other routines.
main :: IO ()
main = do
  -- read command line arguments and defaults for them
  arguments <- cmdArgs exckelArgs

  imageMagick <- findExecutable "convert"
  case imageMagick of
    Nothing -> putStrLn "Could not find imageMagick's \"convert\". Will abbort here."
    Just e -> checkInitial e arguments


-- | Initial check if enough informations are provided and if everything requested makes sense. If
-- | not print an error and exit. If yes, prepare FileInfo for further use and go for next step.
-- | Expansion to absolute paths happening here for some records.
-- |   imC -> path to ImageMagick's convert
-- |   a -> ExckelArgs data structure coming from the initial call
checkInitial :: FilePath -> ExckelArgs -> IO ()
checkInitial imC a = case (wf a, exc a) of
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
    getExcitedStates a fileInfo
  -- one or both of wavefunction and/or logfile are undefined
  _ -> putStrLn "No wavefunction and output file specified. Cannot continue."

-- | Get the excited states from the log file and pass to next step.
getExcitedStates :: ExckelArgs -> FileInfo -> IO ()
getExcitedStates a fi = do
  logRaw <- T.readFile (fi ^. logFile)
  let logParsed = case (fi ^. calcSoftware) of
        -- if guassian, use the gaussian parser
        Gaussian -> parseOnly gaussianLogTDDFT logRaw
  case logParsed of
    -- if parsing went wrong show the error
    Left err -> putStrLn $ "Could not parse the log file with: " ++ show err
    -- if parsing suceeded, pass data to next step
    Right eS -> do
      -- here you could potentially filter the excited states
      let eSfilterByS2 = case (s2Filter a) of
            Nothing      -> eS
            Just contrib -> filterByS2 contrib eS
          eSfilterByFOsc = case (foscFilter a) of
            Nothing -> eSfilterByS2
            Just strength -> filter (\x -> (x ^. oscillatorStrength) >= strength) eSfilterByS2
      doCubes a fi eSfilterByFOsc

-- | Routine to calculate the cubes. Wraps the CubeGenerators. Jumps to next step if no cubes are to
-- | be calculated.
doCubes :: ExckelArgs -> FileInfo -> [ExcState]-> IO ()
doCubes a fi eS = do
  if (nocalccubes a)
    -- cubes shall not be calculated
    then putStrLn "Will not calculate any cube"
    -- cubes shall be calculated
    else do
      case fi ^. cubeGenerator of
        -- program to calculate cubes from wavefunction is MultiWFN
        MultiWFN{} -> do
          case (multiwfn a) of
            -- MultiWFN executable has not been found
            Nothing -> putStrLn "MultiWFN executable not found. Please specify one. Will skip cube calculation."
            -- MultiWFN executable has been found
            Just exe -> do
              let fileInfo = fi
                    & cubeGenerator . cgExePath .~ exe
              CG.MWFN.calculateOrbs fileInfo (nub . concat . map getOrbNumbers $ eS)
              CG.MWFN.calculateCDDs fileInfo [1 .. length eS]
  doPlots a fi eS

-- | Call plotter to visualise all cubes found.
doPlots :: ExckelArgs -> FileInfo -> [ExcState] -> IO ()
doPlots a fi eS = do
  if (norenderimages a)
    -- cubes will not be plotted
    then
      putStrLn "Will not plot cubes and render images"
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
      case fileInfoWithCubes ^. cubePlotter of
        -- VMD is selected as a plotter for cubes
        VMD{} -> do
          case (vmd a, tachyon a) of
            -- the vmd executable has not been found by the intial arguments or defaults
            (Nothing, _) -> putStrLn "VMD executable not found. Please specify one. Will skip cube plotting."
            (Just _, Nothing) -> putStrLn "Tachyon executable not found. Please specify one. Will skip cube plotting."
            -- vmd has been found
            (Just vmdExe, Just tacExe) -> do
              let fileInfoWithVMD = fileInfoWithCubes
                    & cubePlotter . cpExePath .~ vmdExe
                    & cubePlotter . cpStateFile .~ (vmdState a)
                    & cubePlotter . cpStartUp .~ (vmdStartUp a)
                    & cubePlotter . cpTemplate .~ (vmdTemplate a)
                    & cubePlotter . cpRenderer . rExePath .~ tacExe
                    & cubePlotter . cpRenderer . rResolution .~ (imgres a)
                    & cubePlotter . cpRenderer . rImageFormat .~ PNG
              CP.VMD.plotCubes fileInfoWithVMD
  doSummaryDocument a fi eS

-- | Use Pandoc to create the summary document, using all pictures that are there by now.
doSummaryDocument :: ExckelArgs -> FileInfo -> [ExcState] -> IO ()
doSummaryDocument a fi eS = do
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
      orbFileNumbers = map ((read :: String -> Int) . drop 3 . takeBaseName) $ orbImageFiles
      cddFileNumbers = map ((read :: String -> Int) . drop 3 . takeBaseName) $ cddImageFiles
      electronFileNumbers = map ((read :: String -> Int) . drop 8 . takeBaseName) $ electronImageFiles
      holeFileNumbers = map ((read :: String -> Int) . drop 4 . takeBaseName) $ holeImageFiles
      orbImageFilesIndexed = zip orbFileNumbers orbImageFiles
      cddImageFilesIndexed = zip cddFileNumbers cddImageFiles
      electronImageFilesIndexed = zip electronFileNumbers electronImageFiles
      holeImageFilesIndexed = zip holeFileNumbers holeImageFiles
      fileInfoWithImages = fi
        & imageFiles . orbImages .~ Just orbImageFilesIndexed
        & imageFiles . cddImages .~ Just cddImageFilesIndexed
        & imageFiles . electronImages .~ Just electronImageFilesIndexed
        & imageFiles . holeImages .~ Just holeImageFilesIndexed
  --excitationSummary fileInfoWithImages eS
  putStrLn "Gey"
