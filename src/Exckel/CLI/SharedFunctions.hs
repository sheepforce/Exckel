module Exckel.CLI.SharedFunctions
( logMessage
, logHeader
, logInfo
, errMessage
, findAllCubes
, findAllImages
, findAllMRCCMoldenNO
) where
import           Control.Monad
import           Data.Char
import           Data.List.Split
import           Exckel.Types
import           System.Console.ANSI
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf

-- | Put an information to the screen, which displays a value
logMessage :: String -> String -> IO ()
logMessage f s = hPrintf stdout "  %-70s : %-30s\n" f s

-- | Mark start of a section separately
logHeader :: String -> IO ()
logHeader h = do
  setSGR [SetColor Foreground Vivid Green]
  hPutStrLn stdout h
  setSGR [Reset]

-- | Tell something regarding progress of the program, which needs no value given.
logInfo :: String -> IO ()
logInfo i = do
  setSGR [SetColor Foreground Vivid Blue]
  hPutStrLn stdout i
  setSGR [Reset]

-- | Error messages highlighted in red.
errMessage :: String -> IO ()
errMessage x = do
  setSGR [SetColor Foreground Vivid Red]
  hPutStrLn stdout x
  setSGR [Reset]

-- | Look in a given directory for all cubes of required types found.
findAllCubes :: FilePath -> IO CubeFiles
findAllCubes searchPath = do
  searchPathAbs <- makeAbsolute searchPath
  dirContents <- listDirectory searchPathAbs
  let allCubesRel = filter (\x -> (takeExtension x) == ".cube") $ dirContents
  allCubesAbs <- mapM makeAbsolute $ map ((searchPathAbs ++ [pathSeparator]) ++) allCubesRel
  let orbCubesFiles = filter (\x -> (take 3 . takeBaseName $ x) == "orb") allCubesAbs
      natOrbCubeFiles = filter (\x -> (take 6 . takeBaseName $ x) == "natorb") allCubesAbs
      cddCubesFiles = filter (\x -> (take 3 . takeBaseName $ x) == "CDD") allCubesAbs
      electronCubesFiles = filter (\x -> (take 8 . takeBaseName $ x) == "electron") allCubesAbs
      holeCubesFiles = filter (\x -> (take 4 . takeBaseName $ x) == "hole") allCubesAbs
  return CubeFiles
    { _orbCubes      = Just orbCubesFiles
    , _natOrbCubes   = Just natOrbCubeFiles
    , _cddCubes      = Just cddCubesFiles
    , _electronCubes = Just electronCubesFiles
    , _holeCubes     = Just holeCubesFiles
    }

-- | Look for all image files in the directory
findAllImages :: FilePath -> IO ImageFiles
findAllImages searchPath = do
  searchPathAbs <- makeAbsolute searchPath
  dirContents <- listDirectory searchPathAbs
  allImageFiles <- mapM makeAbsolute $
    map ((searchPathAbs ++ [pathSeparator]) ++) .
    filter
      (\x -> (takeExtension x) == "." ++ (map toLower . show $ PNG) ||
             (takeExtension x) == "." ++ (map toLower . show $ JPG)
      ) $ dirContents
  let orbImageFiles = filter (\x -> (take 3 . takeBaseName $ x) == "orb") allImageFiles
      natOrbImageFiles = filter (\x -> (take 6 . takeBaseName $ x) == "natorb") allImageFiles
      cddImageFiles = filter (\x -> (take 3 . takeBaseName $ x) == "CDD") allImageFiles
      electronImageFiles = filter (\x -> (take 8 . takeBaseName $ x) == "electron") allImageFiles
      holeImageFiles = filter (\x -> (take 4 . takeBaseName $ x) == "hole") allImageFiles
      --
      orbFileNumbers = map ((read :: String -> Int) . drop 3 . takeBaseName) $ orbImageFiles
      natOrbNumbers = map (
        ( \[_, sN, oN] -> ((read :: String -> Int) sN, (read :: String -> Int) oN))
        . splitOn "_"
        . takeBaseName
        ) $ natOrbImageFiles
      cddFileNumbers = map ((read :: String -> Int) . drop 3 . takeBaseName) $ cddImageFiles
      electronFileNumbers = map ((read :: String -> Int) . drop 8 . takeBaseName) $ electronImageFiles
      holeFileNumbers = map ((read :: String -> Int) . drop 4 . takeBaseName) $ holeImageFiles
      --
      orbImageFilesIndexed = zip orbFileNumbers orbImageFiles
      natOrbImageFilesIndexed = zipWith (\(sN, oN) im -> (sN, oN, im)) natOrbNumbers natOrbImageFiles
      cddImageFilesIndexed = zip cddFileNumbers cddImageFiles
      electronImageFilesIndexed = zip electronFileNumbers electronImageFiles
      holeImageFilesIndexed = zip holeFileNumbers holeImageFiles
  return ImageFiles
    { _orbImages      = Just orbImageFilesIndexed
    , _natOrbImages   = Just natOrbImageFilesIndexed
    , _cddImages      = Just cddImageFilesIndexed
    , _electronImages = Just electronImageFilesIndexed
    , _holeImages     = Just holeImageFilesIndexed
    }

-- | In a given directory (outdir) look for all natural orbital files of a MRCC calculation with
-- | natural orbitals written to multiple molden files (MOLDEN_NO.xx where xx is the number of the
-- | excited state for which they are valid). It will also sanitise the filenames and make a copy
findAllMRCCMoldenNO :: FilePath -> IO [(Int, FilePath)]
findAllMRCCMoldenNO outdir = do
  dirFilesInit <- (map takeFileName) <$> listDirectory outdir
  mapM_ (\f -> do
    isLink <- pathIsSymbolicLink f
    let hasMoldenExtension = if (takeExtension f) /= ".molden"
          then False
          else True
    if isLink || hasMoldenExtension
      then removeFile f
      else return ()
    ) dirFilesInit
  dirFiles <- (map takeFileName) <$> listDirectory outdir

  let matchingWFFiles = filter (\f -> prefix == (take prefixLength f)) dirFiles
      matchingWFFilesAbs = map (outdir </>) matchingWFFiles
      stateNumbers = map ((read :: String -> Int) . drop 1 . takeExtension) $ matchingWFFilesAbs
      saneWFFileNamesAbs = map (++ ".molden") matchingWFFilesAbs
      result = zip stateNumbers saneWFFileNamesAbs
  zipWithM_ createFileLink matchingWFFilesAbs saneWFFileNamesAbs
  return result
  where
    prefix = "MOLDEN_NO."
    prefixLength = length prefix
