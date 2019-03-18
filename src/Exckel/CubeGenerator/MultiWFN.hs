{-
Wrapper to MultiWFN. This has been tested only with the Gnu compatibel version 3.4.0 from
https://github.com/stecue/gMultiwfn/releases .
-}
module Exckel.CubeGenerator.MultiWFN
( calculateOrbs
, calculateCDDs
) where
import           Control.Monad
import           Data.List.Split
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf

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

  mwfnLogFile <- openFile (mwfnOutDir ++ [pathSeparator] ++ "MultiWFN.out") WriteMode
  mwfnErrFile <- openFile (mwfnOutDir ++ [pathSeparator] ++ "MultiWFN.err") WriteMode
  hSetBuffering mwfnLogFile LineBuffering
  hSetBuffering mwfnErrFile LineBuffering

  hPutStrLn mwfnInput "200" -- other functions
  -- hPutStrLn mwfnInput "3"
  mapM_ (hPutStrLn mwfnInput) $
    map (\o -> unlines
          [ "3"             -- Generate cube file for multiple orbital wavefunctions
          , printOrbList o  -- chunks of 10 orbitals to be plotted
          , "3"             -- High quality grid
          , "1"             -- Output the grid data of these orbitals as separate cube files
          ]
        ) (chunksOf 10 orbInds)
  -- hPutStrLn mwfnInput $ printOrbList orbInds
  -- hPutStrLn mwfnInput "3"   -- High quality grid
  -- hPutStrLn mwfnInput "1"   -- Output the grid data of these orbitals as separate cube files
  hPutStrLn mwfnInput ""
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
    orbGeneratorMWFN = case (fi ^. orbGenerator) of
      Nothing -> error "You requested Multiwfn calls but MultiWFN has not been found. This should not happen."
      Just og -> og
    mwfnPath = orbGeneratorMWFN ^. ogExePath
    mwfnWFN = fi ^. waveFunctionFile
    mwfnOutDir = fi ^. outputPrefix
    printOrbList :: [Int] -> String
    printOrbList o = init . concat . map ((++ ",") . show) $ o
    oldCubeNames = map (++ ".cub") . map ((mwfnOutDir ++ [pathSeparator] ++ "orb") ++) $ (map (printf "%06d") orbInds)
    newCubeNames = map (++ ".cube") . map ((mwfnOutDir ++ [pathSeparator] ++ "orb") ++) $ (map show orbInds)

-- | Calculate charge difference densities, holes and electrons from excitations. Be aware, that
-- | this only works with Gaussian and ORCA output files, which should be provided. The function
-- | will cras, if the logfile is a Nothing but print a hint first
calculateCDDs :: FileInfo -> [Int] -> IO ()
calculateCDDs fi esN = do
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

    mwfnLogFile <- openFile (mwfnOutDir ++ [pathSeparator] ++ "MultiWFN.out") WriteMode
    mwfnErrFile <- openFile (mwfnOutDir ++ [pathSeparator] ++ "MultiWFN.err") WriteMode
    hSetBuffering mwfnLogFile LineBuffering
    hSetBuffering mwfnErrFile LineBuffering

    hPutStrLn mwfnInput "18"  -- Electron excitation analysis
    hPutStrLn mwfnInput "1"   -- Analyze and visualize hole-electron distribution, ...
    hPutStrLn mwfnInput $ fi ^. logFile
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
  where
    cddGeneratorMWFN = case (fi ^. cddGenerator) of
      Nothing -> error "You requested Multiwfn calls but MultiWFN has not been found. This should not happen."
      Just cg -> cg
    mwfnPath = cddGeneratorMWFN ^. cddExePath
    mwfnWFN = fi ^. waveFunctionFile
    mwfnOutDir = fi ^. outputPrefix
    oldCDDName = mwfnOutDir ++ [pathSeparator] ++ "CDD.cub"
    newCDDName n = mwfnOutDir ++ [pathSeparator] ++ "CDD" ++ show n ++ ".cube"
    oldElectronName = mwfnOutDir ++ [pathSeparator] ++ "electron.cub"
    newElectronName n = mwfnOutDir ++ [pathSeparator] ++ "electron" ++ show n ++ ".cube"
    oldHoleName = mwfnOutDir ++ [pathSeparator] ++ "hole.cub"
    newHoleName n = mwfnOutDir ++ [pathSeparator] ++ "hole" ++ show n ++ ".cube"
