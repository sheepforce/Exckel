{-
This module creates an internal pandoc representaion of the summary, that shall be generated from
the excited state informations
-}

{-# LANGUAGE OverloadedStrings #-}
module Exckel.DocumentCreator
(
) where
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text              as T
import qualified Data.Vector            as V
import           Exckel.Types
import           Lens.Micro.Platform
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Printf


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
       -- heading of the tablem user "headerWith"
       [ header 1 "State"
       , header 1 "Transition"
       , header 1 "Weight / %"
       , header 1 "E / eV"
       , header 1 "λ / nm"
       , header 1 "f_osc"
       ]
       (tableContents es)
  where
    -- title of the Pandoc document
    title :: Many Inline
    title = fromList [Str ("Excited State")]
    -- create the [[Blocks]] scheme (rows first, columns second) required for the Pandoc table from
    -- excited state output
    tableContents :: [ExcState] -> [[Blocks]]
    tableContents excStates =
      map (\e -> [ para . text . show $ e ^. nState
                 , manyCIEntry (e ^. ciWavefunction)
                 , manyWeightEntry (e ^. ciWavefunction)
                 , para . text . printf "%4.2F" . (* 27.11386020) $ e ^. relEnergy
                 , para . text . printf "%4.2F" . (1239.84197386209 /) . (* 27.11386020) $ e ^. relEnergy
                 , para . text . printf "%6.4F" $ e ^. oscillatorStrength
                 ]
          ) excStates
    -- fill single line in a transition block, which constructs an CI determinant (CIS has single
    -- pair per line, CID has two pairs per line and so on)
    ciEntry :: CIDeterminant -> String
    ciEntry ciD =
      concatMap (\x ->
        -- fromOrb
        (show (x ^. fromOrb . _1)) ++
        (case (x ^. fromOrb . _2) of
          Just s  -> if s == Alpha then "A" else "B"
          Nothing -> ""
        ) ++
        " → " ++
        (show (x ^. toOrb . _1)) ++
        (case (x ^. toOrb . _2) of
          Just s  -> if s == Alpha then "A" else "B"
          Nothing -> ""
        ) ++
        if (length (ciD ^. excitationPairs) > 1)
          then " & "
          else ""
      ) $ ciD ^. excitationPairs
    -- CI wavefunction written by individual CI determinants (one per line), each line is a CI
    -- determinant
    manyCIEntry :: V.Vector CIDeterminant -> Blocks
    manyCIEntry ciDs = lineBlock . map text . V.toList . V.map ciEntry $ ciDs
    -- for a CI determinant give the weight in the complete CI wavefunction
    weightEntry :: CIDeterminant -> String
    weightEntry ciD = printf "%6.4F"(ciD ^. coeff)
    -- line by line the weights of the CI determinants in the CI wavefunction
    manyWeightEntry :: V.Vector CIDeterminant -> Blocks
    manyWeightEntry ciDs = lineBlock . map text . V.toList . V.map weightEntry $ ciDs



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
      Just d  -> liftIO . putStrLn $ "Data directory is: " ++ show dataDir
      Nothing -> liftIO . putStrLn $ "Could not get a data directory"
    odtTemplate <- getDefaultTemplate "odt"
    writeODT def { writerTemplate = Just odtTemplate} testDoc


testIt :: ExcState -> IO ()
testIt es = do
  pandocWriteResult <- testPan
  case pandocWriteResult of
    Right r -> B.writeFile "/user/seeber/Test.odt" r
    Left _  -> putStrLn "Oi, something went wrong"
