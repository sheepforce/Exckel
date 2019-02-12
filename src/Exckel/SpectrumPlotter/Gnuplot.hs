{-
Gnuplot wrapper interface. Rather than using the gnuplot library, i decided to go with a system call
as documentation of the gnuplot library is rather bad (aka i have no idea how to use it). File names
here are hardcoded and "Spectrum.png" is assumed (in the rest of the program) to be succesfully
created by this module.
-}
module Exckel.SpectrumPlotter.Gnuplot
( plotSpectrum
) where
import           Exckel.ExcUtils
import           Exckel.Types
import           Lens.Micro.Platform
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process
import           Text.Printf

-- | Plots the spectrum to a png file. Uses the informations about excited states. It takes
-- |   - fi -> the FileInfo data type for paths
-- |   - es -> a list of excited states (should be the ones filtered by S**2 value and energy range).
-- |           Will plot the spectrum from all of these states
-- |   - fes -> a list of excited states filtered by oscillator strength (should be a subset of es
-- |            and the states filtered by oscillator strength AND S**2 value from the CLI)
plotSpectrum :: FileInfo -> [ExcState] -> [ExcState] -> IO ()
plotSpectrum fi es fes = do
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

  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Peaks.dat") (makeTable3 peaksLabeled)
  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Conv_Epsilon.dat") (makeTable2 convolutedSpectrumEpsilon)
  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Conv_FOsc.dat") (makeTable2 convolutedSpectrumFOsc)
  writeFile (outDir ++ [pathSeparator] ++ "Spectrum_Filtered_Peaks.dat") (makeTable3 peaksFiltredLabeled)

  gnuplotLogFile <- openFile (outDir ++ [pathSeparator] ++ "Gnuplot.out") WriteMode
  gnuplotErrFile <- openFile (outDir ++ [pathSeparator] ++ "Gnuplot.err") WriteMode
  hSetBuffering gnuplotLogFile LineBuffering
  hSetBuffering gnuplotErrFile LineBuffering

  gnuLog <- hGetContents gnuplotOutput
  gnuErr <- hGetContents gnuplotError

  hPutStrLn gnuplotInput $ unlines
    [ "set terminal png size 1200,800 font \"NotoSans\""
    , "set grid"
    , "set link x2 via 1239.84197386209/x inverse 1239.84197386209/x"
    , "set xtics nomirror"
    , "set xlabel \"E / eV\""
    , "set x2label \"{/Symbol l} / nm\""
    , "set x2tics"
    , "set ytics nomirror"
    , "set y2tics"
    , "set y2label \"{/Symbol e} / l mol^{-1} cm^{-1}\""
    , "set ylabel \"f_{osc}\""
    , "set link y2 via 1.3062974e8*y/(" ++ show σ ++ "* 8065.54400545911) inverse y*(" ++ show σ ++ "*8065.54400545911)/1.3062974e8"
    , "set output \"Spectrum.png\""
    , "set xrange [" ++ show eMin ++ ":" ++ show eMax ++ "]"
    , "plot \"" ++ outDir ++ [pathSeparator] ++ "Spectrum_Peaks.dat" ++     "\" using 1:2 with impulses linetype 1 notitle , \\"
    , "     \"" ++ outDir ++ [pathSeparator] ++ "Spectrum_Conv_FOsc.dat" ++ "\" using 1:2 with lines linetype 1 notitle , \\"
    , "     \"" ++ outDir ++ [pathSeparator] ++ "Spectrum_Filtered_Peaks.dat" ++ "\" using 1:2:3 with labels offset char 0,1 notitle"
    , "exit"
    ]
  print (eMin, eMax)

  hPutStrLn gnuplotLogFile gnuLog
  hPutStrLn gnuplotErrFile gnuErr

  hClose gnuplotLogFile
  hClose gnuplotErrFile

  exitCode <- waitForProcess gnuplotProcH
  cleanupProcess (Just gnuplotInput, Just gnuplotOutput, Just gnuplotError, gnuplotProcH)
  where
    outDir = fi ^. outputPrefix
    fwhm = fi ^. spectrumPlotter . spBroadening
    σ = fwhm / (2 * sqrt(log 2))
    peaksEnergy = map hartree2eV . map (^. relEnergy) $ es
    peaksFOsc = map (^. oscillatorStrength) es
    peaks = zip peaksEnergy peaksFOsc
    labels = map (^. nState) $ es
    peaksLabeled = zip3 peaksEnergy peaksFOsc labels
    (eMin, eMax) = case (fi ^. spectrumPlotter . spERange) of
      Nothing -> ((minimum $ peaksEnergy) - 0.5, (maximum $ peaksEnergy) + 0.5)
      Just (a, b) -> (a, b)
    grid = [eMin, eMin + 0.01 .. eMax]
    convolutedSpectrumFOsc = convolutionSum (gauss fwhm) peaks grid
    convolutedSpectrumEpsilon =
      map (\(e, f) -> (e, oscStrength2Epsilon fwhm f)) $ convolutedSpectrumFOsc
    peaksFilteredEnergy = map hartree2eV . map (^. relEnergy) $ fes
    peaksFilteredFOsc = map (^. oscillatorStrength) $ fes
    peaksFilteredlabels = map (^. nState) $ fes
    peaksFiltredLabeled = zip3 peaksFilteredEnergy peaksFilteredFOsc peaksFilteredlabels

makeTable2 :: PrintfArg a => [(a, a)] -> String
makeTable2 spec = concatMap (\(a, b) -> printf "%8.4F    %8.4F\n" a b) spec

makeTable3 :: (PrintfArg a, PrintfArg b) => [(a, a, b)] -> String
makeTable3 spec = concatMap (\(a, b, c) -> printf "%8.4F    %8.4F    %3d\n" a b c) spec
