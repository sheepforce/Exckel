module Exckel.SpectrumPlotter.SharedFunctions
( createDataFiles
) where
import qualified Data.Text           as T
import           Exckel.ExcUtils
import           Exckel.Types
import           Lens.Micro.Platform
import           Text.Printf

createDataFiles :: FileInfo -> [ExcState] -> [ExcState] -> (T.Text, T.Text, T.Text, T.Text)
createDataFiles fi es fes =
  ( makeTable3 peaksLabeled
  , makeTable2 convolutedSpectrumEpsilon
  , makeTable2 convolutedSpectrumFOsc
  , makeTable3 peaksFiltredLabeled
  )
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

makeTable2 :: PrintfArg a => [(a, a)] -> T.Text
makeTable2 spec = T.pack $ concatMap (\(a, b) -> printf "%12.8F    %12.8F\n" a b) spec

makeTable3 :: (PrintfArg a, PrintfArg b) => [(a, a, b)] -> T.Text
makeTable3 spec = T.pack $ concatMap (\(a, b, c) -> printf "%12.8F    %12.8F    %3d\n" a b c) spec
