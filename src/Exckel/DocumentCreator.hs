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


excitationTable :: ExcState -> Pandoc
excitationTable es = setTitle title $ doc $
     para "Hey, this is just a pandoc test"
  <> para "It's a sheep!"
  where
    title :: Many Inline
    title = fromList [Str ("Excited State" ++ show (es ^. nState))]

----------------------------------------------------------------------------------------------------
-- Testing Pandoc functionality
----------------------------------------------------------------------------------------------------


testDoc :: Pandoc
testDoc = setTitle "Test Document" $ doc $
  para "Hey, this is just a pandoc test"


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
