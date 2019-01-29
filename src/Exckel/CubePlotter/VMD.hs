{-# LANGUAGE OverloadedStrings #-}
module Exckel.CubePlotter.VMD
( plotCubes
) where
import           Control.Applicative
import           Control.Monad.Identity
import           Data.Attoparsec.Text
import qualified Data.HashMap.Lazy      as H
import           Data.List              hiding (length)
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
  vmdStartupFile <- case (fi ^. cubePlotter . cpStartUp) of
    Nothing -> do
      homeDir <- getHomeDirectory
      hasVMDRC <- doesFileExist (homeDir ++ "/.vmdrc")
      if hasVMDRC
        then do
          -- cleanup the vmdrc as it contains the annoying "after idle" statement
          vmdrcRaw <- T.readFile (homeDir ++ "/.vmdrc")
          let vmdrcRmOpen = T.lines . T.replace "after idle {" "" $ vmdrcRaw
              vmdrcLength = length vmdrcRmOpen
              vmdrcRmClosed = deleteNth (vmdrcLength - 3) vmdrcRmOpen
              vmdrcClean = T.unlines vmdrcRmClosed
          return vmdrcClean
        else return ""
    Just f -> T.readFile f
  T.writeFile (vmdOutDir ++ "/VMDStartup.tcl") vmdStartupFile

  -- read the Tcl template script, make replacements and write it to work outputPrefix with the name
  -- PlotVMD.tcl
  substitueTemplate fi

  -- launch a VMD process
  createDirectoryIfMissing True vmdOutDir
  (Just vmdInput, Just vmdOutput, Just vmdError, vmdProcH) <-
    createProcess (proc (fi ^. cubePlotter . cpExePath) [ "-eofexit"
                                                        , "-startup", (vmdOutDir ++ "/VMDStartup.tcl")
                                                        , "-e", vmdOutDir ++ "/PlotVMD.tcl"
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

  vmdLogFile <- openFile (vmdOutDir ++ "/VMD.out") WriteMode
  vmdErrFile <- openFile (vmdOutDir ++ "/VMD.err") WriteMode
  hSetBuffering vmdLogFile LineBuffering
  hSetBuffering vmdErrFile LineBuffering

  vmdLog <- hGetContents vmdOutput
  vmdErr <- hGetContents vmdError

  hPutStr vmdLogFile vmdLog
  hPutStr vmdErrFile vmdErr

  hClose vmdLogFile
  hClose vmdErrFile

  exitCode <- waitForProcess vmdProcH
  cleanupProcess (Just vmdInput, Just vmdOutput, Just vmdError, vmdProcH)

  -- call renderer if specified
  case (fi ^. cubePlotter . cpRenderer) of
    Tachyon {} -> do
      -- iterate over all written tachyon files ("t") and over all tachyon images after
      -- rendering ("ri") and the imagemagick results
      mapM_ (\(t, ri, imi) -> do
        -- call Tachyon on each input
        (Just tacInput, Just tacOutput, Just tacError, tacProcH) <- createProcess
          (proc (fi ^. cubePlotter . cpRenderer . rExePath)
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
        writeFile (vmdOutDir ++ "/Tachyon.out") tacLog
        writeFile (vmdOutDir ++ "/Tachyon.err") tacErr
        exitCode <- waitForProcess tacProcH
        cleanupProcess (Just tacInput, Just tacOutput, Just tacError, tacProcH)

        -- after tachyon has finished, trim with imagemagick's convert
        (Nothing, Nothing, Nothing, convertProcH) <- createProcess
          (proc (fi ^. imConvertExePath) [ri, "-trim", imi])
          {cwd = Just vmdOutDir}
        exitCode <- waitForProcess convertProcH
        cleanupProcess (Nothing, Nothing, Nothing, convertProcH)

        -- delete tachyon images with borders
        removeFile ri
        ) (zip3 tachyonInputs tachyonImages imagemagickImages)
  where
    vmdOutDir = fi ^. outputPrefix
    tachyonInputs =
      (map ((++ ".dat") . T.unpack) . getBaseNames . concat) $
      [ fromMaybe [] $ fi ^. cubeFiles . orbCubes
      , fromMaybe [] $ fi ^. cubeFiles . cddCubes
      , fromMaybe [] $ fi ^. cubeFiles .electronCubes
      , fromMaybe [] $ fi ^. cubeFiles . holeCubes
      ]
    resX = fst $ fi ^. cubePlotter . cpRenderer . rResolution
    resY = snd $ fi ^. cubePlotter . cpRenderer . rResolution
    imageFormat = fi ^. cubePlotter . cpRenderer . rImageFormat
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
  stateRaw <- case (fi ^. cubePlotter . cpStateFile) of
    Just p  -> T.readFile p
    Nothing -> return ""
  let stateParsed = parseOnly vmdState stateRaw
      viewpoint = ("set viewpoints" `T.append`) <$> stateParsed

  -- get basenames of all cubes
  let orbitalBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . orbCubes)
      cddBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . cddCubes)
      electronBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . electronCubes)
      holeBaseNames = toTclList $ getBaseNames <$> (fi ^. cubeFiles . holeCubes)

  -- key value map for substitutions by Ginger as Map
  let context = H.fromList
        [ ("orbs", orbitalBaseNames)
        , ("cdddens", cddBaseNames `T.append` electronBaseNames `T.append` holeBaseNames)
        , ("viewpoint", case viewpoint of
             Right v -> v `T.append` (T.unlines ["", "unset viewplist"])
             Left _  -> ""
          )
        ] :: H.HashMap T.Text T.Text

  -- read the Tcl template script for VMD, parse it and make substitutions (using Ginger)
  templateRaw <- readFile (fi ^. cubePlotter . cpTemplate)
  let templateParsed = runIdentity $
        parseGinger
          (const $ return Nothing) -- no resolver being used, as no includes necessary
          Nothing                  -- source name
          templateRaw
      templateOutput = easyRender context <$> templateParsed
  case templateOutput of
    Right t -> T.writeFile ((fi ^. outputPrefix) ++ "/PlotVMD.tcl") t
    Left e  -> print e

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
