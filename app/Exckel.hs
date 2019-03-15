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
  args <- cmdArgs exckelArgs
  -- Initialise all fields with environment and command line argument related stuff
  fileInfo1 <- initialise args
  -- Parse and filter the excited states.
  (excitedStatesSpectrum, excitedStatesAnalysis) <- getExcitedStates fileInfo1
  -- Plot the spectrum

  putStrLn "Hey sheep"
