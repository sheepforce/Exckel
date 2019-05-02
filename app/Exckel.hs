import qualified Data.ByteString.Char8  as BS
import           Exckel.CLI.CLI
import           Exckel.CmdArgs
import           Exckel.EmbedContents
import           System.Console.ANSI
import           System.Console.CmdArgs hiding (def)

main :: IO ()
main = do
  -- Print the Exckel logo
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn . BS.unpack $ exckelLogo
  setSGR [Reset]
  -- Read command line arguments and find out which mode is beeing used
  arguments <- cmdArgs (modes [exckelArgs, tabulateArgs])
  case arguments of
    -- Normal Exckel run for excited state summary
    Exckel {} -> do
      -- Initialise all fields with environment and command line argument related stuff
      fileInfoInitial <- initialise arguments
      -- Parse and filter the excited states.
      (excitedStatesSpectrum, excitedStatesSpectrumLabel, excitedStatesAnalysis, resortMap) <- getExcitedStates fileInfoInitial
      -- Plot the spectrum
      plotSpectrum fileInfoInitial (excitedStatesSpectrum, excitedStatesSpectrumLabel) resortMap
      -- Calculate orbital cubes
      fileInfoUpdatedOrbCubes <- calcOrbCubes fileInfoInitial excitedStatesAnalysis
      -- Calculate CDD cubes
      fileInfoUpdatedCDDCubes <- calcCDDCubes fileInfoUpdatedOrbCubes excitedStatesAnalysis
      -- Render all cubes to images
      fileInfoWithImages <- doPlots fileInfoUpdatedCDDCubes
      -- Create the summary document
      createSummaryDocument fileInfoWithImages excitedStatesAnalysis resortMap
    -- Tabulate arbitrary data in a docx document
    Tabulate {} -> do
      tabulate arguments
