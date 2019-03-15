import Exckel.Types
import Exckel.CLI.Initialise

main :: IO ()
main = do
  -- Print the Exckel logo
  setSGR [SetColor Foreground Vivid Yellow]
  putStrLn $ BS.unpack exckelLogo
  setSGR [Reset]
  -- Read command line arguments
  args <- cmdArgs exckelArgs
  -- Initialise all fields with environment and command line argument related stuff
  fileInfo1 <- initialise args
  -- 
