import Exckel.Types
import Exckel.CLI.CLI
import qualified Data.ByteString.Char8 as BS
import           System.Console.ANSI
import           System.Console.CmdArgs           hiding (def)
import Exckel.EmbedContents
import Exckel.CmdArgs

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
  --
  print fileInfo1
