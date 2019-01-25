import           Data.Attoparsec.Text
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Exckel.CmdArgs
import           Exckel.CubeGenerator.MultiWFN as CG.MWFN
import           Exckel.Parser
import           Exckel.Types
import           Lens.Micro.Platform
import           Paths_Exckel
import           System.Console.CmdArgs        hiding (def)
import           System.Directory

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
getExcitedStates a f = do
  logRaw <- T.readFile (f ^. logFile)
  let logParsed = case (f ^. calcSoftware) of
        -- if guassian, use the gaussian parser
        Gaussian -> parseOnly gaussianLogTDDFT logRaw
  case logParsed of
    -- if parsing went wrong show the error
    Left e -> putStrLn $ "Could not parse the log file with: " ++ show e
    -- if parsing suceeded, pass data to next step
    Right e -> do
      -- here you could potentially filter the excited states (and filter the before)
      let excitedStates = e
      doCubes a f excitedStates

-- | Routine to calculate the cubes. Wraps the CubeGenerators. Jumps to next step if no cubes are to
-- | be calculated.
doCubes :: ExckelArgs -> FileInfo -> [ExcState]-> IO ()
doCubes a f e = do
  if (nocalccubes a)
    -- cubes shall not be calculated
    then putStrLn "Will not calculate any cube"
    -- cubes shall be calculated
    else do
      case f ^. cubeGenerator of
        -- program to calculate cubes from wavefunction is MultiWFN
        MultiWFN{} -> do
          case (multiwfn a) of
            -- MultiWFN executable has not been found
            Nothing -> putStrLn "MultiWFN executable not found. Please specify one."
            -- MultiWFN executable has been found
            Just e -> do
              let fileInfo = f
                    & cubeGenerator . cgExePath .~ e
              CG.MWFN.calculateOrbs fileInfo [1..10]
              CG.MWFN.calculateCDDs fileInfo [1..10]
