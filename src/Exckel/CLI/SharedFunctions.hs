module Exckel.CLI.SharedFunctions
( logMessage
, logHeader
, logInfo
, errMessage
, findAllCubes
, findAllImages
, findAllMRCCMoldenNO
, sortOrbCubes
, popReplace
) where
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.List.Extra     hiding (splitOn)
import           Data.List.Split
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Exckel.Types
import           Lens.Micro.Platform hiding (to)
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
    { _orbCubes      = orbCubesFiles
    , _natOrbCubes   = natOrbCubeFiles
    , _cddCubes      = cddCubesFiles
    , _electronCubes = electronCubesFiles
    , _holeCubes     = holeCubesFiles
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
      orbImageFilesIndexed = M.fromList $ zip orbFileNumbers orbImageFiles
      natOrbImageFilesIndexed = M.fromList $ zip natOrbNumbers natOrbImageFiles
      cddImageFilesIndexed = M.fromList $ zip cddFileNumbers cddImageFiles
      electronImageFilesIndexed = M.fromList $ zip electronFileNumbers electronImageFiles
      holeImageFilesIndexed = M.fromList $ zip holeFileNumbers holeImageFiles
  return ImageFiles
    { _orbImages      = orbImageFilesIndexed
    , _natOrbImages   = natOrbImageFilesIndexed
    , _cddImages      = cddImageFilesIndexed
    , _electronImages = electronImageFilesIndexed
    , _holeImages     = holeImageFilesIndexed
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

-- | Sort the orbitals and natural orbitals by their numbers and states, so that they appear in a
-- | more helpful order in the output document.
sortOrbCubes :: CubeFiles -> CubeFiles
sortOrbCubes cf = cf
  & orbCubes .~ orbitalsSorted
  & natOrbCubes .~ naturalOrbitalsSorted
  where
    orbitals = cf ^. orbCubes
    orbitalIndices = map ((read :: String -> Int) . drop 3 . takeBaseName) $ orbitals
    orbitalsIndexed = zip orbitalIndices orbitals
    orbitalsSorted = map snd . sortOn fst $ orbitalsIndexed
    naturalOrbitals = cf ^. natOrbCubes
    naturalOrbitalsIndices =
      map (
        (\[_, state, orb] -> ((read :: String -> Int) state, (read :: String -> Int) orb)) .
        splitOn "_" .
        takeBaseName
      ) naturalOrbitals
    naturalOrbitalsIndexed = zip naturalOrbitalsIndices naturalOrbitals
    naturalOrbitalsSorted =
      map snd .
      concat .
      map (sortOn (^. _1 . _2)) .
      sortOn ((^. _1 . _1) . head) .
      groupOn (^. _1 . _1) .
      sortOn (^. _1 . _1) $
      naturalOrbitalsIndexed

-- | In case the order of excited states has changed because of renumbering of them, this function
-- | takes care of changing the ImageFiles type, so that the renumbered images are linked with the
-- | images which are on file system level with the old states. The first argument is a replacement
-- | list of old state numbers and new state numbers.
{-
linkRenameImages :: Map Int Int -> ImageFiles -> ImageFiles
linkRenameImages rMap images = images
  & natOrbImages . _1 .~ natOrbsStateNew
  & holeImages . _1 .~ holeIndsNew
  & electronImages . _1 .~ electronIndsNew
  &
  where
    -- Get state numbers from all relevant image types.
    natOrbsStateInds = images ^. natOrbImages . _1
    holeInds = images ^. holeImages . _1
    electronInds = images ^. electronImages . _1
    cddInds = images ^. cddInds . _1
    -- Assign new state numbers
    natOrbsStateNew = popReplace rMap natOrbsStateInds
    holeIndsNew = popReplace rMap holeInds
    electronIndsNew = popReplace rMap electronInds
    cddIndsNew = popReplace rMap cddInds
-}

-- | Replace patterns in a list without duplication issues. This avoids issues with possible
-- | duplicate keys by accepting only Map as input. Keeps the original order of the input list
-- | otherwise.
popReplace :: (Eq a, Ord a) => Map a a -> [a] -> [a]
popReplace rMap oldList =
  [ joinReplaceColumn (map (!! i) singlySubstLists)
  | i <- [0 .. length oldList - 1]
  ]
  where
    -- Convert the lookup map to a list of tuples.
    rList = M.toList rMap
    -- Given a single substitution, look for the search pattern in list A. If it is found there, write
    -- append the result of the substitution to an accumulator list B.
    popReplaceElementsAug :: Eq a => (a, a) -> [a] -> [(Bool, a)]
    popReplaceElementsAug (from, to) oL =
      reverse $ foldl (\acc x -> if x == from then (True, to):acc else (False, x):acc) [] oL
    -- Multiple original lists, each with a single replacement are calculated. Each element is
    -- now augmented with a True if it has been replaced or a False if not. It will then join the
    -- lists again so that all replacements are available in the final result.
    joinReplaceColumn :: [(Bool, a)] -> a
    joinReplaceColumn r = if all (\(b, _) -> b == False) r
      then snd . head $ r
      else snd . head . filter (\(b, _) -> b == True) $ r
    -- Versions of the original list, where one replacement each has been performed.
    singlySubstLists = zipWith (\r l -> popReplaceElementsAug r l) rList (repeat oldList)
