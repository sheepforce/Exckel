module Exckel.CubeGenerator.MultiWFN
( calculateOrbs
, calculateCDDs
) where
import Exckel.Types
import System.Process
import System.IO
import Lens.Micro.Platform
import System.Directory
import Control.Monad
import Text.Printf

-- | Given the file system informations, create orbital cubes for the relevant orbitals (given by
-- | indices) and rename them to orbN.cube
calculateOrbs :: FileInfo -> [Int] -> IO ()
calculateOrbs fi orbInds = do
  createDirectoryIfMissing True mwfnOutDir

  (Just mwfnInput, Just mwfnOutput, Just mwfnError, mwfnProcH) <-
    createProcess (proc mwfnPath [mwfnWFN])
    { std_out = CreatePipe
    , std_in = CreatePipe
    , std_err = CreatePipe
    , cwd = Just mwfnOutDir
    }

  hSetBuffering mwfnInput LineBuffering
  hSetBuffering mwfnOutput LineBuffering
  hSetBuffering mwfnError LineBuffering

  mwfnLogFile <- openFile (mwfnOutDir ++ "/MultiWFN.out") WriteMode
  mwfnErrFile <- openFile (mwfnOutDir ++ "/MultiWFN.err") WriteMode
  hSetBuffering mwfnLogFile LineBuffering
  hSetBuffering mwfnErrFile LineBuffering

  hPutStrLn mwfnInput "200" -- other functions
  hPutStrLn mwfnInput "3"   -- Generate cube file for multiple orbital wavefunctions
  hPutStrLn mwfnInput $ printOrbList orbInds
  hPutStrLn mwfnInput "3"   -- High quality grid
  hPutStrLn mwfnInput "1"   -- Output the grid data of these orbitals as separate cube files
  hPutStrLn mwfnInput "0"   -- Return (to main menu)
  hPutStrLn mwfnInput "-10" -- hidden option to exit gracefully in the main menu

  mwfnLog <- hGetContents mwfnOutput
  mwfnErr <- hGetContents mwfnError

  hPutStr mwfnLogFile mwfnLog
  hPutStr mwfnErrFile mwfnErr

  hClose mwfnLogFile
  hClose mwfnErrFile

  exitCode <- waitForProcess mwfnProcH
  cleanupProcess (Just mwfnInput, Just mwfnOutput, Just mwfnError, mwfnProcH)

  zipWithM_ renameFile oldCubeNames newCubeNames
  where
    mwfnPath = fi ^. cubeGenerator . cgExePath
    mwfnWFN = fi ^. waveFunctionFile
    mwfnOutDir = fi ^. outputPrefix
    printOrbList :: [Int] -> String
    printOrbList o = init . concat . map ((++ ",") . show) $ o
    oldCubeNames = map (++ ".cub") . map ((mwfnOutDir ++ "/orb") ++) $ (map (printf "%06d") orbInds)
    newCubeNames = map (++ ".cube") . map ((mwfnOutDir ++ "/orb") ++) $ (map show orbInds)

-- | Calculate charge difference densities, holes and electrons from excitations. Be aware, that
-- | this only works with Gaussian and ORCA output files, which should be provided. The function
-- | will cras, if the logfile is a Nothing but print a hint first
calculateCDDs :: FileInfo -> [Int] -> IO ()
calculateCDDs fi esN = do
  case mwfnLog of
    Just l -> do
      createDirectoryIfMissing True mwfnOutDir
      mapM_ (\n -> do
        (Just mwfnInput, Just mwfnOutput, Just mwfnError, mwfnProcH) <-
          createProcess (proc mwfnPath [mwfnWFN])
          { std_out = CreatePipe
          , std_in = CreatePipe
          , std_err = CreatePipe
          , cwd = Just mwfnOutDir
          }

        hSetBuffering mwfnInput LineBuffering
        hSetBuffering mwfnOutput LineBuffering
        hSetBuffering mwfnError LineBuffering

        mwfnLogFile <- openFile (mwfnOutDir ++ "/MultiWFN.out") WriteMode
        mwfnErrFile <- openFile (mwfnOutDir ++ "/MultiWFN.err") WriteMode
        hSetBuffering mwfnLogFile LineBuffering
        hSetBuffering mwfnErrFile LineBuffering

        hPutStrLn mwfnInput "18"  -- Electron excitation analysis
        hPutStrLn mwfnInput "1"   -- Analyze and visualize hole-electron distribution, ...
        hPutStrLn mwfnInput $ l
        hPutStrLn mwfnInput $ show n -- excited state to analyse
        hPutStrLn mwfnInput "1"   -- Visualize and analyze hole, electron and transition density ...
        hPutStrLn mwfnInput "3"   -- High quality grid
        hPutStrLn mwfnInput "10"  -- Output cube file of hole distribution to current folder
        hPutStrLn mwfnInput "1"   -- Total (local term + cross term)
        hPutStrLn mwfnInput "11"  -- Output cube file of electron distribution to current folder
        hPutStrLn mwfnInput "1"   -- Total (local term + cross term)
        hPutStrLn mwfnInput "15"  -- Output cube file of charge density difference to current folder
        hPutStrLn mwfnInput "0"   -- Return
        hPutStrLn mwfnInput "0"   -- Return
        hPutStrLn mwfnInput "0"   -- Return
        hPutStrLn mwfnInput "-10" -- hidden option to exit gracefully in the main menu

        mwfnLog <- hGetContents mwfnOutput
        mwfnErr <- hGetContents mwfnError

        hPutStr mwfnLogFile mwfnLog
        hPutStr mwfnErrFile mwfnErr

        hClose mwfnLogFile
        hClose mwfnErrFile

        exitCode <- waitForProcess mwfnProcH
        cleanupProcess (Just mwfnInput, Just mwfnOutput, Just mwfnError, mwfnProcH)

        renameFile oldCDDName (newCDDName n)
        renameFile oldElectronName (newElectronName n)
        renameFile oldHoleName (newHoleName n)
            ) esN
    Nothing -> do
      putStrLn "You have not specified a log file with excitations (ORCA or Gaussian log)."
      putStrLn "Cannot proceed without. Therefore doing nothing"
  where
    mwfnPath = fi ^. cubeGenerator . cgExePath
    mwfnWFN = fi ^. waveFunctionFile
    mwfnLog = fi ^. logFile
    mwfnOutDir = fi ^. outputPrefix
    oldCDDName = mwfnOutDir ++ "/CDD.cub"
    newCDDName n = mwfnOutDir ++ "/CDD" ++ show n ++ ".cube"
    oldElectronName = mwfnOutDir ++ "/electron.cub"
    newElectronName n = mwfnOutDir ++ "/electron" ++ show n ++ ".cube"
    oldHoleName = mwfnOutDir ++ "/hole.cub"
    newHoleName n = mwfnOutDir ++ "/hole" ++ show n ++ ".cube"
