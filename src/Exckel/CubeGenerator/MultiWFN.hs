module Exckel.CubeGenerator.MultiWFN
( calculateOrbs
) where
import Exckel.Types
import System.Process
import System.IO
import Lens.Micro.Platform
import System.Directory
import Control.Monad
import Text.Printf


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

  hPutStrLn mwfnInput "200" -- 200 other functions
  hPutStrLn mwfnInput "3"   -- 3 Generate cube file for multiple orbital wavefunctions
  hPutStrLn mwfnInput $ printOrbList orbInds
  hPutStrLn mwfnInput "3"   -- 3 High quality grid
  hPutStrLn mwfnInput "1"   -- 1 Output the grid data of these orbitals as separate cube files
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
