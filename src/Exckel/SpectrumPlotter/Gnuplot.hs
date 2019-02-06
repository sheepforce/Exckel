module Exckel.SpectrumPlotter.Gnuplot
(
) where
import System.Directory
import System.IO
import System.Process
import Exckel.Types
import Lens.Micro.Platform
import Exckel.ExcUtils
import Text.Printf
import System.FilePath

plotSpectrum :: FileInfo -> [ExcState] -> IO ()
plotSpectrum fi es = do
  (Just gnuplotInput, Just gnuplotOutput, Just gnuplotError, gnuplotProcH) <-
    createProcess (proc (fi ^. spectrumPlotter . spExePath) [])
    { std_out = CreatePipe
    , std_in = CreatePipe
    , std_err = CreatePipe
    , cwd = Just outDir
    }

  hSetBuffering gnuplotInput LineBuffering
  hSetBuffering gnuplotOutput LineBuffering
  hSetBuffering gnuplotError LineBuffering

  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Peaks.dat") (makeTable peaks)
  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Conv_Epsilon.dat") (makeTable convolutedSpectrumEpsilon)
  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Conv_FOsc.dat") (makeTable convolutedSpectrumFOsc)

  gnuplotLogFile <- openFile (outDir ++ [pathSeparator] ++ "Gnuplot.out") WriteMode
  gnuplotErrFile <- openFile (outDir ++ [pathSeparator] ++ "Gnuplot.err") WriteMode
  hSetBuffering gnuplotLogFile LineBuffering
  hSetBuffering gnuplotErrFile LineBuffering

  gnuLog <- hGetContents gnuplotOutput
  gnuErr <- hGetContents gnuplotError

  hPutStrLn gnuplotLogFile gnuLog
  hPutStrLn gnuplotErrFile gnuErr

  hClose gnuplotLogFile
  hClose gnuplotErrFile

  exitCode <- waitForProcess gnuplotProcH
  cleanupProcess (Just gnuplotInput, Just gnuplotOutput, Just gnuplotError, gnuplotProcH)
  where
    outDir = fi ^. outputPrefix
    fwhm = fi ^. spectrumPlotter . spBroadening
    peaksEnergy = map hartree2eV . map (^. relEnergy) $ es
    peaksFOsc = map (^. oscillatorStrength) es
    peaks = zip peaksEnergy peaksFOsc
    (eMin, eMax) = case (fi ^. spectrumPlotter . spERange) of
      Nothing -> (((-) 0.5) . minimum $ peaksEnergy, (+0.5) . maximum $ peaksEnergy)
      Just (a, b) -> (a, b)
    grid = [eMin, eMin + 0.01 .. eMax]
    convolutedSpectrumFOsc = convolutionSum (gauss fwhm) peaks grid
    convolutedSpectrumEpsilon =
      map (\(e, f) -> (e, oscStrength2Epsilon fwhm f)) $ convolutedSpectrumFOsc

makeTable :: PrintfArg a => [(a, a)] -> String
makeTable spec = concatMap (\(a, b) -> printf "%8.4F    %8.4F\n" a b) spec
