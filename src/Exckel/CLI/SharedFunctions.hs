module Exckel.CLI.SharedFunctions
( logMessage
, logHeader
, logInfo
, errMessage
, findAllCubes
) where
import           System.Console.ANSI
import           System.Console.CmdArgs hiding (def)
import           System.IO
import           Text.Printf
import System.FilePath
import System.Directory
import Exckel.Types

-- | Put an information to the screen, which displays a value
logMessage f s = printf "  %-70s : %-30s\n" f s

-- | Mark start of a section separately
logHeader h = do
  setSGR [SetColor Foreground Vivid Green]
  hPutStrLn stdout h
  setSGR [Reset]

-- | Tell something regarding progress of the program, which needs no value given.
logInfo i = do
  setSGR [SetColor Foreground Vivid Blue]
  hPutStrLn stdout i
  setSGR [Reset]

-- | Error messages highlighted in red.
errMessage x = do
  setSGR [SetColor Foreground Vivid Red]
  hPutStrLn stdout x
  setSGR [Reset]

-- | Look in a given directory for all cubes of required types found.
findAllCubes :: FilePath -> IO CubeFiles
findAllCubes searchPath = do
  searchPathAbs <- makeAbsolute searchPath
  dirContents <- listDirectory searchPathAbs
  let allCubesRel = filter (\x -> (takeExtension x) == ".cube") $ outDirContents
  allCubesAbs <- mapM makeAbsolute $ map (searchPathAbs ++ [pathSeparator] ++) allCubes
  let orbCubesFiles = filter (\x -> (take 3 . takeBaseName $ x) == "orb") allCubesAbs
      cddCubesFiles = filter (\x -> (take 3 . takeBaseName $ x) == "CDD") allCubesAbs
      electronCubesFiles = filter (\x -> (take 8 . takeBaseName $ x) == "electron") allCubesAbs
      holeCubesFiles = filter (\x -> (take 4 . takeBaseName $ x) == "hole") allCubesAbs
  return CubeFiles
    { _orbCubes      = Just orbCubesFiles
    , _cddCubes      = Just cddCubesFiles
    , _electronCubes = Just electronCubesFiles
    , _holeCubes     = Just electronCubesFiles
    }

-- | Look for all image files in the directory
findAllImages :: FilePath -> IO ImageFiles
findAllImages searchPath = do
  searchPathAbs <- makeAbsolute searchPath
  dirContents <- listDirectory searchPathAbs
  allImageFiles <- mapM makeAbsolute $
    map (searchPathAbs ++ [pathSeparator] ++) .
    filter
      (\x -> (takeExtension x) == "." ++ (map toLower . show $ PNG) ||
             (takeExtension x) == "." ++ (map toLower . show $ JPG)
      ) $ dirContents
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
  return ImageFiles
    { _orbImages      = Just orbImageFilesIndexed
    , _cddImages      = Just cddImageFilesIndexed
    , _electronImages = Just electronImageFilesIndexed
    , _holeImages     = Just holeImageFilesIndexed
    }
