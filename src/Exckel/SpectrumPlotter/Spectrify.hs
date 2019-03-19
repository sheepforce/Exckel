{-

-}
module Exckel.SpectrumPlotter.Spectrify
( plotSpectrum
) where
import qualified Data.ByteString.Char8                  as B
import qualified Data.Text.IO                           as T
import           Exckel.EmbedContents
import           Exckel.SpectrumPlotter.SharedFunctions
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

-- | Plots the spectrum to a png file. Uses the informations about excited states. It takes
-- |   - fi -> the FileInfo data type for paths
-- |   - es -> a list of excited states (should be the ones filtered by S**2 value and energy range).
-- |           Will plot the spectrum from all of these states
-- |   - fes -> a list of excited states filtered by oscillator strength (should be a subset of es
-- |            and the states filtered by oscillator strength AND S**2 value from the CLI)
plotSpectrum :: FileInfo -> [ExcState] -> [ExcState] -> IO ()
plotSpectrum fi es fes = do
  let (peaks, convolutedEpsilon, convolutedFosc, filteredPeaks) = createDataFiles fi es fes
  T.writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Peaks.dat") peaks
  T.writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Conv_Epsilon.dat") convolutedEpsilon
  T.writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Conv_FOsc.dat") convolutedFosc
  T.writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Filtered_Peaks.dat") filteredPeaks

  B.writeFile (outDir ++ [pathSeparator] ++ "spectrify.py") spectrifyPy
  permOfSpectrify <- getPermissions (outDir ++ [pathSeparator] ++ "spectrify.py")
  setPermissions (outDir ++ [pathSeparator] ++ "spectrify.py") (permOfSpectrify {executable = True})

  (Just spectrifyInput, Just spectrifyOutput, Just spectrifyError, spectrifyProcH) <-
    createProcess ( proc (outDir ++ [pathSeparator] ++ "spectrify.py")
                         [ "--exckel"
                         , outDir ++ [pathSeparator] ++ "Spectrum_Conv_FOsc.dat"
                         , outDir ++ [pathSeparator] ++ "Spectrum_Filtered_Peaks.dat"
                         , outDir ++ [pathSeparator] ++ "Spectrum_Peaks.dat"
                         , "--exckel-grid"
                         , "--exckel-color"
                         , "#ff5000"
                         , "--no-plot"
                         , "--top-eV"
                         , "--spectrum-file-extension"
                         , "png"
                         , "--hwhh"
                         , show (σ/2.0)
                         , "--peak-labels"
                         ]
                  )
    { std_out = CreatePipe
    , std_in = CreatePipe
    , std_err = CreatePipe
    , cwd = Just outDir
    }

  hSetBuffering spectrifyOutput LineBuffering
  hSetBuffering spectrifyError LineBuffering

  spectrifyLogFile <- openFile (outDir ++ [pathSeparator] ++ "Spectrify.out") WriteMode
  spectrifyErrFile <- openFile (outDir ++ [pathSeparator] ++ "Spectrify.err") WriteMode

  hSetBuffering spectrifyLogFile LineBuffering
  hSetBuffering spectrifyErrFile LineBuffering

  spectrifyLog <- hGetContents spectrifyOutput
  spectrifyErr <- hGetContents spectrifyError

  hPutStrLn spectrifyLogFile spectrifyLog
  hPutStrLn spectrifyErrFile spectrifyErr

  hClose spectrifyLogFile
  hClose spectrifyErrFile

  _ <- waitForProcess spectrifyProcH
  cleanupProcess (Just spectrifyInput, Just spectrifyOutput, Just spectrifyError, spectrifyProcH)

  renameFile (outDir ++ [pathSeparator] ++ "spectra.png") (outDir ++ [pathSeparator] ++ "Spectrum.png")
  where
    outDir = fi ^. outputPrefix
    fwhm = fi ^. spectrumPlotter . spBroadening
    σ = fwhm / (2 * sqrt(log 2))
