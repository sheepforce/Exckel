{-
Wrapper to MultiWFN. This has been tested only with the Gnu compatibel version 3.4.0 from
https://github.com/stecue/gMultiwfn/releases .
-}
module Exckel.CubeGenerator.MultiWFN
( calculateOrbs
, calculateCDDs
) where
import           Control.Monad
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Exckel.CLI.SharedFunctions
import           Exckel.ExcUtils
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf

-- | From an excited state get a list of all orbitals to plot to visualise this state
orbitalsToPlot :: [ExcState] -> [Int]
orbitalsToPlot es = nub . concat . map getOrbNumbers $ es

-- | Given a list of natural orbital wavefunction files
assignWFFileToState :: [(Int, FilePath)] -> [ExcState] -> [(ExcState, FilePath)]
assignWFFileToState lWFs es =
  map fromJust .
  filter isJust $
  zipWith (\(i, path) s -> if i == (s ^. nState)
    then Just (s, path)
    else Nothing
  ) sortedFiles sortedStates
  where
    relevantStateNumbers = map (^. nState) $ es
    sortedStates = sortOn (\s -> s ^. nState) es
    sortedFiles = sortOn fst . filter (\file -> fst file `elem` relevantStateNumbers) $ lWFs

-- | Calculate orbitals from a single or multiple wavefunction files. If a single wavefunction file
-- | is given, the results are assumed to be orbitals, but if multiple wavefunction files for
-- | specific states are given, they are assumed to be state specific natural orbitals and will be
-- | named accordingly.
calculateOrbs :: FileInfo -> [ExcState] -> IO ()
calculateOrbs fi es = do
  case (fi ^. waveFunctionFile) of
    Left singleWFFile  -> do
      let orbInds = orbitalsToPlot es
      calculateOrbsFromFile fi singleWFFile orbInds
      let oldCubeNames =
            map (mwfnOutDir </>) $
            map (\i -> "orb" ++ (printf "%06d" i) ++ ".cub") orbInds
          newCubeNames =
            map (mwfnOutDir </>) $
            map (\i -> "orb" ++ (show i) ++ ".cube") orbInds
      zipWithM_ renameFile oldCubeNames newCubeNames
    Right multiWFFiles -> do
      let assignedStates = assignWFFileToState multiWFFiles es
      mapM_ (\(state, path) -> do
          let orbInds = orbitalsToPlot [state]
          logInfo $ "State " ++ show (state ^. nState) ++ " with orbitals " ++ show orbInds
          calculateOrbsFromFile fi path orbInds
          let oldCubeNames =
                map (mwfnOutDir </>) $
                map (\i -> "orb" ++ (printf "%06d" i) ++ ".cub") orbInds
              newCubeNames =
                map (mwfnOutDir </>) $
                map (\i -> "natorb_" ++ (show (state ^. nState)) ++ "_" ++ (show i) ++ ".cube") orbInds
          zipWithM_ renameFile oldCubeNames newCubeNames
        ) assignedStates
  where
    mwfnOutDir = fi ^. outputPrefix

-- | Given the file system informations, create orbital cubes for the relevant orbitals (given by
-- | indices) and rename them to orbN.cube
calculateOrbsFromFile :: FileInfo -> FilePath -> [Int] -> IO ()
calculateOrbsFromFile fi wfFile orbInds = do
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

  _ <- waitForProcess mwfnProcH
  cleanupProcess (Just mwfnInput, Just mwfnOutput, Just mwfnError, mwfnProcH)
  where
    orbGeneratorMWFN = case (fi ^. orbGenerator) of
      Nothing -> error "You requested Multiwfn calls but MultiWFN has not been found. This should not happen."
      Just og -> og
    mwfnPath = orbGeneratorMWFN ^. ogExePath
    mwfnWFN = wfFile
    mwfnOutDir = fi ^. outputPrefix
    printOrbList :: [Int] -> String
    printOrbList o = init . concat . map ((++ ",") . show) $ o

calculateCDDs :: FileInfo -> [ExcState] -> IO ()
calculateCDDs fi es = do
  case (fi ^. waveFunctionFile) of
    Left singleWFFile  -> do
      calculateCDDsFromFile fi singleWFFile (map (^. nState) es)
    Right multiWFFiles -> do
      let assignedStates = assignWFFileToState multiWFFiles es
      mapM_ (\(state, path) -> do
          calculateCDDsFromFile fi path [state ^. nState]
        ) assignedStates
-- | Calculate charge difference densities, holes and electrons from excitations. Be aware, that
-- | this only works with Gaussian and ORCA output files, which should be provided. The function
-- | will crash, if the logfile is a Nothing but print a hint first
calculateCDDsFromFile :: FileInfo -> FilePath -> [Int] -> IO ()
calculateCDDsFromFile fi wfFile esN = do
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

    hPutStrLn mwfnInput "18"     -- Electron excitation analysis
    hPutStrLn mwfnInput "1"      -- Analyze and visualize hole-electron distribution, ...
    hPutStrLn mwfnInput $ fi ^. logFile
    hPutStrLn mwfnInput $ show n -- excited state to analyse
    hPutStrLn mwfnInput "1"      -- Visualize and analyze hole, electron and transition density ...
    hPutStrLn mwfnInput "3"      -- High quality grid
    hPutStrLn mwfnInput "10"     -- Output cube file of hole distribution to current folder
    hPutStrLn mwfnInput "1"      -- Total (local term + cross term)
    hPutStrLn mwfnInput "11"     -- Output cube file of electron distribution to current folder
    hPutStrLn mwfnInput "1"      -- Total (local term + cross term)
    hPutStrLn mwfnInput "15"     -- Output cube file of charge density difference to current folder
    hPutStrLn mwfnInput "0"      -- Return
    hPutStrLn mwfnInput "0"      -- Return
    hPutStrLn mwfnInput "0"      -- Return
    hPutStrLn mwfnInput "-10"    -- hidden option to exit gracefully in the main menu

    mwfnLog <- hGetContents mwfnOutput
    mwfnErr <- hGetContents mwfnError

    hPutStr mwfnLogFile mwfnLog
    hPutStr mwfnErrFile mwfnErr

    hClose mwfnLogFile
    hClose mwfnErrFile

    _ <- waitForProcess mwfnProcH
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
    mwfnWFN = wfFile
    mwfnOutDir = fi ^. outputPrefix
    oldCDDName = mwfnOutDir ++ [pathSeparator] ++ "CDD.cub"
    newCDDName n = mwfnOutDir ++ [pathSeparator] ++ "CDD" ++ show n ++ ".cube"
    oldElectronName = mwfnOutDir ++ [pathSeparator] ++ "electron.cub"
    newElectronName n = mwfnOutDir ++ [pathSeparator] ++ "electron" ++ show n ++ ".cube"
    oldHoleName = mwfnOutDir ++ [pathSeparator] ++ "hole.cub"
    newHoleName n = mwfnOutDir ++ [pathSeparator] ++ "hole" ++ show n ++ ".cube"
