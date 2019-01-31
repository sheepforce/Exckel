{-
This module creates an internal pandoc representaion of the summary, that shall be generated from
the excited state informations. it uses all available data, which means all correctly named images
found in the filesystem and writes docx or odt summary documents.
-}
module Exckel.DocumentCreator
( excitationSummary
) where
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Vector            as V
import           Exckel.Types           hiding (def)
import           Lens.Micro.Platform
import           Text.Pandoc            hiding (FileInfo)
import           Text.Pandoc.Builder    hiding (FileInfo)
import           Text.Printf
import           Exckel.Parser
import Data.List.Split (chunksOf)
import Data.Maybe

-- | This generates a Pandoc document as a summary of an excited state calculation. It takes a
-- | (possibly filtered) list of excited states to summarise in a document.
excitationSummary :: FileInfo -> [ExcState] -> Pandoc
excitationSummary fi es =
  (setTitle title)
  $ doc $
       table
         "Excited state summary" -- caption
         -- column alignments and width
         [ (AlignCenter, 0.1)    -- state number
         , (AlignCenter, 0.2)    -- orbital pairs
         , (AlignRight, 0.125)   -- weight
         , (AlignRight, 0.075)   -- energy
         , (AlignRight, 0.1)     -- wavelength
         , (AlignRight, 0.1)     -- oscillator strength
         , (AlignCenter, 0.15)   -- hole
         , (AlignCenter, 0.15)   -- electron
         ]
         -- heading of the table
         [ header 1 "State"
         , header 1 "Transition"
         , header 1 "Weight / %"
         , header 1 "E / eV"
         , header 1 "λ / nm"
         , header 1 "f_osc"
         , header 1 "hole"
         , header 1 "electron"
         ]
         -- generated summary for excitations
         (tableContents es)
    <> table
         "Orbital"                        -- caption
         (replicate 5 (AlignCenter, 0.2)) -- 5 columns
         []                               -- empty headers
         (orbContents)                    -- pictures of the orbitals

  where
    -- title of the Pandoc document
    title :: Many Inline
    title = fromList [Str ("Excited State Summary")]
    -- create the [[Blocks]] scheme (rows first, columns second) required for the Pandoc table from
    -- excited state output
    tableContents :: [ExcState] -> [[Blocks]]
    tableContents excStates =
      map (\e -> [ para . text . show $ e ^. nState
                 , manyCIEntry (e ^. ciWavefunction)
                 , manyWeightEntry (e ^. ciWavefunction)
                 , para . text . printf "%4.2F" . (* 27.11386020) $ e ^. relEnergy
                 , para . text . printf "%4.1F" . (1239.84197386209 /) . (* 27.11386020) $ e ^. relEnergy
                 , para . text . printf "%6.4F" $ e ^. oscillatorStrength
                 , case (getCDDImageByType fi e holeImages) of
                     Nothing -> para . text $ ""
                     Just (nState, imagePath) -> para $ imageWith ("", ["align-left"], [("width", "2.5cm")]) imagePath "" (text . ("hole " ++) . show $ nState)
                 , case (getCDDImageByType fi e electronImages) of
                     Nothing -> para . text $ ""
                     Just (nState, imagePath) -> para $ imageWith ("", ["align-left"], [("width", "2.5cm")]) imagePath "" (text . ("electron " ++) . show $ nState)
                 ]
          ) excStates
    -- create a table of all orbital pictures
    orbContents :: [[Blocks]]
    orbContents =
      chunksOf 5 .
      map (\i -> para $ imageWith ("", ["align-left"], [("width", "2.5cm")]) (snd i) "" (text . show . fst $ i)) .
      fromMaybe [] $
      fi ^. imageFiles . orbImages
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
    -- Look for hole image of the excited state. Give the FileInfo type, an excited state and the
    -- lens for the corresponding image type (from ImageFiles type).
    getCDDImageByType fi eS lens = case candidates of
      Nothing -> Nothing
      Just [] -> Nothing
      Just x -> Just $ head x
      where
        candidates =
          filter (\x -> (fst x) == (eS ^. nState)) <$>
          fi ^. imageFiles . lens
