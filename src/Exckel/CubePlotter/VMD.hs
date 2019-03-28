{-
Wrapper to VMD. Plot all properly named cubes found on filesystem and render them with tachyon.
Appearance of the results is determined by 3 files: First, a vmdrc like file is read to set
everything like in a normal VMD startup. Sets most parameters. Then, optioanally, a state file is
read to get the perspective and. Last, a TCL template script is loaded and templates replaced
(filenames of cubes and perspective), fed into VMD and then executed. Tachyon will be called to
render the images.
-}
module Exckel.CubePlotter.VMD
( plotCubes
) where
import           Control.Monad.Identity
import           Data.Attoparsec.Text
import qualified Data.HashMap.Lazy      as H
import           Data.Maybe
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Exckel.Parser
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Ginger            hiding (length)

plotCubes :: FileInfo -> IO ()
plotCubes fi = do
  -- If a startup file for vmd is specified, take this one. If not, look for the vmdrc in the home
  -- directory for the vmdrc file and clean it up, so that it can be used. If this also does not
  -- exist, live with the defaults of VMD by writing an empty startup file
  vmdStartupFile <- case (cubePlotterVMD ^. cpStartUp) of
    Nothing -> do
      homeDir <- getHomeDirectory
      hasVMDRC <- doesFileExist (homeDir ++ [pathSeparator] ++ ".vmdrc")
      if hasVMDRC
        then do
          -- cleanup the vmdrc as it contains the annoying "after idle" statement
          vmdrcRaw <- T.readFile (homeDir ++ [pathSeparator] ++ ".vmdrc")
          let vmdrcRmOpen = T.lines . T.replace "after idle {" "" $ vmdrcRaw
              vmdrcLength = length vmdrcRmOpen
              vmdrcRmClosed = deleteNth (vmdrcLength - 3) vmdrcRmOpen
              vmdrcClean = T.unlines vmdrcRmClosed
          return vmdrcClean
        else return ""
    Just f -> T.readFile f
  T.writeFile (vmdOutDir ++ [pathSeparator] ++ "VMDStartup.tcl") vmdStartupFile

  -- read the Tcl template script, make replacements and write it to work outputPrefix with the name
  -- PlotVMD.tcl
  substitueTemplate fi

  -- launch a VMD process
  createDirectoryIfMissing True vmdOutDir
  (Just vmdInput, Just vmdOutput, Just vmdError, vmdProcH) <-
    createProcess (proc (cubePlotterVMD ^. cpExePath) [ "-eofexit"
                                                      , "-startup", (vmdOutDir ++ [pathSeparator] ++ "VMDStartup.tcl")
                                                      , "-e", vmdOutDir ++ [pathSeparator] ++ "PlotVMD.tcl"
                                                      ]
                  )
    { std_out = CreatePipe
    , std_in = CreatePipe
    , std_err = CreatePipe
    , cwd = Just vmdOutDir
    }

  hSetBuffering vmdInput LineBuffering
  hSetBuffering vmdOutput LineBuffering
  hSetBuffering vmdError LineBuffering

  vmdLogFile <- openFile (vmdOutDir ++ [pathSeparator] ++ "VMD.out") WriteMode
  vmdErrFile <- openFile (vmdOutDir ++ [pathSeparator] ++ "VMD.err") WriteMode
  hSetBuffering vmdLogFile LineBuffering
  hSetBuffering vmdErrFile LineBuffering

  vmdLog <- hGetContents vmdOutput
  vmdErr <- hGetContents vmdError

  hPutStr vmdLogFile vmdLog
  hPutStr vmdErrFile vmdErr

  hClose vmdLogFile
  hClose vmdErrFile

  _ <- waitForProcess vmdProcH
  cleanupProcess (Just vmdInput, Just vmdOutput, Just vmdError, vmdProcH)

  -- call renderer if specified
  case (cubePlotterVMD ^. cpRenderer) of
    Tachyon {} -> do
      -- iterate over all written tachyon files ("t") and over all tachyon images after
      -- rendering ("ri") and the imagemagick results
      mapM_ (\(t, ri, imi) -> do
        -- call Tachyon on each input
        (Just tacInput, Just tacOutput, Just tacError, tacProcH) <- createProcess
          (proc (cubePlotterVMD ^. cpRenderer . rExePath)
            [ t -- Tachyon input file
            , "-o",  ri
            , "-res", show resX, show resY
            , "-aasamples", "12"
            , "-format", show imageFormat
            ]
          )
          { std_out = CreatePipe
          , std_in = CreatePipe
          , std_err = CreatePipe
          , cwd = Just vmdOutDir
          }
        tacLog <- hGetContents tacOutput
        tacErr <- hGetContents tacError
        writeFile (vmdOutDir ++ [pathSeparator] ++ "Tachyon.out") tacLog
        writeFile (vmdOutDir ++ [pathSeparator] ++ "Tachyon.err") tacErr
        _ <- waitForProcess tacProcH
        cleanupProcess (Just tacInput, Just tacOutput, Just tacError, tacProcH)

        -- after tachyon has finished, trim with imagemagick's convert
        (Nothing, Nothing, Nothing, convertProcH) <- createProcess
          (proc (cubePlotterVMD ^. cpRenderer . rIMExePath) [ri, "-trim", imi])
          {cwd = Just vmdOutDir}
        _ <- waitForProcess convertProcH
        cleanupProcess (Nothing, Nothing, Nothing, convertProcH)

        -- delete tachyon images with borders
        removeFile ri
        ) (zip3 tachyonInputs tachyonImages imagemagickImages)
  where
    cubePlotterVMD = case (fi ^. cubePlotter) of
      Nothing -> error "You have requested cube plotting with VMD but VMD and a Renderer were not specified."
      Just cp -> cp
    vmdOutDir = fi ^. outputPrefix
    tachyonInputs =
      (map ((++ ".dat") . T.unpack) . getBaseNames . concat) $
      [ fromMaybe [] $ fi ^. cubeFiles . orbCubes
      , fromMaybe [] $ fi ^. cubeFiles . natOrbCubes
      , fromMaybe [] $ fi ^. cubeFiles . cddCubes
      , fromMaybe [] $ fi ^. cubeFiles .electronCubes
      , fromMaybe [] $ fi ^. cubeFiles . holeCubes
      ]
    resX = fst $ cubePlotterVMD  ^. cpRenderer . rResolution
    resY = snd $ cubePlotterVMD ^. cpRenderer . rResolution
    imageFormat = cubePlotterVMD ^. cpRenderer . rImageFormat
    imageSuffix = T.unpack . ("." `T.append`) . T.toLower . T.pack . show $ imageFormat
    tachyonImages = map ((++ imageSuffix) . (++ "_tachyon") . fst . splitExtension) tachyonInputs
    imagemagickImages = map ((++ imageSuffix) . fst . splitExtension) tachyonInputs


-- | Read parse and substitute an VMD plot script template. It takes care of 3 variables in the
-- | template script, based on values in the FileInfo data type. Namely these are:
-- |   - orbs : a list of filepaths to the orbital cubes but without the .cube suffix
-- |   - cdddens : a list of filepaths to the CDD, electron and hole cubes without the .cube suffix
-- |   - viewpoint : a vmd state command to select camera position and perspective
substitueTemplate :: FileInfo -> IO ()
substitueTemplate fi = do
  -- read the VMD state file (if set) to get the viewpoint settings
  stateRaw <- case (cubePlotterVMD ^. cpStateFile) of
    Just p  -> T.readFile p
    Nothing -> return ""
  let stateParsed = parseOnly vmdState stateRaw
      viewpoint = ("set viewpoints" `T.append`) <$> stateParsed

  -- get basenames of all cubes
  let orbitalBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . orbCubes)
      natOrbBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . natOrbCubes)
      cddBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . cddCubes)
      electronBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . electronCubes)
      holeBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . holeCubes)

  -- key value map for substitutions by Ginger as Map
  let context = H.fromList
        [ ("orbs", orbitalBaseNames `T.append` natOrbBaseNames)
        , ("cdddens", cddBaseNames `T.append` electronBaseNames `T.append` holeBaseNames)
        , ("viewpoint", case viewpoint of
             Right v -> v `T.append` (T.unlines ["", "unset viewplist"])
             Left _  -> ""
          )
        ] :: H.HashMap T.Text T.Text

  -- read the Tcl template script for VMD, parse it and make substitutions (using Ginger)
  let templateRaw = T.unpack $ cubePlotterVMD ^. cpTemplate :: String
      templateParsed = runIdentity $
        parseGinger
          (const $ return Nothing) -- no resolver being used, as no includes necessary
          Nothing                  -- source name
          templateRaw
      templateOutput = easyRender context <$> templateParsed
  case templateOutput of
    Right t -> T.writeFile ((fi ^. outputPrefix) ++ [pathSeparator] ++ "PlotVMD.tcl") t
    Left e  -> print e
  where
    cubePlotterVMD = case (fi ^. cubePlotter) of
      Nothing -> error "You have requested cube plotting with VMD but VMD and a Renderer were not specified."
      Just cp -> cp

-- | From a list of filepaths, get their basenames
getBaseNames :: [FilePath] -> [T.Text]
getBaseNames = map (T.pack . fst . splitExtension)

-- | Convert a list of basenames to a single string for the "foreach" loop in the tcl script
toTclList :: Maybe [T.Text] -> T.Text
toTclList s = case s of
  Just x  -> T.concat . map (`T.append` " ") $ x
  Nothing -> ""

-- | Delete Nth element from a list
deleteNth :: Int -> [a] -> [a]
deleteNth n l = let (il, tl) = splitAt n l in il ++ tail (tl)
