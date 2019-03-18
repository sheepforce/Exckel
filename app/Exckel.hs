import qualified Data.ByteString.Char8  as BS
import           Exckel.CLI.CLI
import           Exckel.CmdArgs
import           Exckel.EmbedContents
import           Exckel.Types
import           System.Console.ANSI
import           System.Console.CmdArgs hiding (def)

main :: IO ()
main = do
  -- Print the Exckel logo
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn . BS.unpack $ exckelLogo
  setSGR [Reset]
  -- Read command line arguments
  arguments <- cmdArgs exckelArgs
  -- Initialise all fields with environment and command line argument related stuff
  fileInfoInitial <- initialise arguments
  -- Parse and filter the excited states.
  (excitedStatesSpectrum, excitedStatesSpectrumLabel, excitedStatesAnalysis) <- getExcitedStates fileInfoInitial
  -- Plot the spectrum
  plotSpectrum fileInfoInitial (excitedStatesSpectrum, excitedStatesSpectrumLabel)
  -- Calculate orbital cubes
  fileInfoUpdatedOrbCubes <- calcOrbCubes fileInfoInitial excitedStatesAnalysis


  putStrLn "Hey sheep"
