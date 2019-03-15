module Exckel.CLI.SharedFunctions
( logMessage
, logHeader
, logInfo
, errMessage
) where
import           Text.Printf (printf)
import           System.Console.ANSI
import           System.IO


-- | Put an information to the screen, which displays a value
logMessage f s = printf "  %-70s : %-30s\n" f s

-- | Mark start of a section separately
logHeader h = do
  setSGR [SetColor Foreground Vivid Green]
  hPutStrLn stdout h
  setSGR [Reset]

-- | Tell something regarding progress of the program, which needs no value given.
logInfo i = do
  setSGR [SetColor Foreground Vivid Blue]
  hPutStrLn stdout i
  setSGR [Reset]

-- | Error messages highlighted in red.
errMessage x = do
  setSGR [SetColor Foreground Vivid Red]
  hPutStrLn stdout x
  setSGR [Reset]
