{-
This module creates an internal pandoc representaion of the summary, that shall be generated from
the excited state informations
-}

{-# LANGUAGE OverloadedStrings #-}
module Exckel.DocumentCreator
(
) where
import           Control.Monad.IO.Class
import           Exckel.Types
import           Lens.Micro.Platform
import           Text.Pandoc
import           Text.Pandoc.Builder
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T


excitationTable :: [ExcState] -> Pandoc
excitationTable es =
    (setTitle title)
  $ doc $
     para "Hey, this is just a pandoc test"
  <> para "It's a sheep!"
  <> table
       "Excited state summary" -- caption
       -- column alignments and width
       [ (AlignCenter, 0.1)    -- state number
       , (AlignCenter, 0.4)    -- orbital pairs
       , (AlignRight, 0.2)     -- weight
       , (AlignRight, 0.1)     -- energy
       , (AlignRight, 0.1)     -- wavelength
       , (AlignRight, 0.1)     -- oscillator strength
       ]
       -- heading of the table
       [ header 1 "State"
       , header 1 "Transition"
       , header 1 "Weight / %"
       , header 1 "E / eV"
       , header 1 "λ / nm"
       , header 1 "f_osc"
       ]
       [replicate 5 (para "Hey")
       ]
  where
    title :: Many Inline
    title = fromList [Str ("Excited State")]

----------------------------------------------------------------------------------------------------
-- Testing Pandoc functionality
----------------------------------------------------------------------------------------------------


testDoc :: Pandoc
testDoc = setTitle "Test Document" $ doc $
     para "Hey, this is just a pandoc test"
  <> para "And this is a sheep"


testPan :: IO (Either PandocError B.ByteString)
testPan = runIO $ do
    setUserDataDir (Just "/user/seeber/Downloads/pandoc-master/data")
    dataDir <- getUserDataDir
    case dataDir of
      Just d -> liftIO . putStrLn $ "Data directory is: " ++ show dataDir
      Nothing -> liftIO . putStrLn $ "Could not get a data directory"
    odtTemplate <- getDefaultTemplate "odt"
    writeODT def { writerTemplate = Just odtTemplate} testDoc


testIt :: ExcState -> IO ()
testIt es = do
  pandocWriteResult <- testPan
  case pandocWriteResult of
    Right r -> B.writeFile "/user/seeber/Test.odt" r
    Left _ -> putStrLn "Oi, something went wrong"
