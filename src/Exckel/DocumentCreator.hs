{-
This module creates an internal pandoc representaion of the summary, that shall be generated from
the excited state informations. it uses all available data, which means all correctly named images
found in the filesystem and writes docx or odt summary documents.
-}
module Exckel.DocumentCreator
( excitationSummary
) where
import           Data.List.Split     (chunksOf)
import qualified Data.Map.Strict     as M
import qualified Data.Vector         as V
import           Exckel.ExcUtils
import           Exckel.Types
import           Lens.Micro.Platform
import           System.FilePath
import           Text.Pandoc         hiding (FileInfo)
import           Text.Pandoc.Builder
import           Text.Printf

-- | This generates a Pandoc document as a summary of an excited state calculation. It takes a
-- | (possibly filtered) list of excited states to summarise in a document.
excitationSummary :: FileInfo -> [ExcState] -> Pandoc
excitationSummary fi es =
  (setTitle title)
  $ doc $
    para (imageWith ("", ["align-center"], [("width", "17cm")]) spectrumPath "" "")
    <>
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
       , header 1 "Weight"
       , header 1 "E / eV"
       , header 1 "λ / nm"
       , header 1 "f_osc"
       , header 1 "hole"
       , header 1 "electron"
       ]
       -- generated summary for excitations
       (tableContents wfes)
    <>
    table
      "Orbital"                        -- caption
      (replicate 5 (AlignCenter, 0.2)) -- 5 columns
      []                               -- empty headers
      ( orbContents                    -- pictures of the orbitals
         ( (^. nBasisFunctions) . head $ wfes)
         ( case ((^. wfType) . head $ wfes) of
             Nothing                  -> False
             Just ClosedShell         -> False
             Just OpenShell           -> True
             Just RestrictedOpenShell -> True
         )
      )

  where
    -- title of the Pandoc document
    title :: Many Inline
    title = fromList [Str ("Excited State Summary")]
    -- hard coded name of the image file containing the spectrum
    spectrumPath = (fi ^. outputPrefix) ++ [pathSeparator] ++ "Spectrum.png"
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
                     (_, Nothing)              -> para . text $ ""
                     (nState', Just imagePath) -> para $ imageWith ("", ["align-center"], [("width", "2.2cm")]) imagePath "" (text . ("hole " ++) . show $ nState')
                 , case (getCDDImageByType fi e electronImages) of
                     (_, Nothing)              -> para . text $ ""
                     (nState', Just imagePath) -> para $ imageWith ("", ["align-center"], [("width", "2.2cm")]) imagePath "" (text . ("electron " ++) . show $ nState')
                 ]
          ) excStates
    -- create a table of all (natural) orbital pictures
    orbContents :: Int -> Bool -> [[Blocks]]
    orbContents nBasFun isOpenShell = case (fi ^. waveFunctionFile) of
      Left _  ->
        chunksOf 5 .
        map (\i -> para (imageWith ("", ["align-center"], [("width", "2.5cm")]) (snd i) (show . fst $ i) (text . show . fst $ i))
                   <>
                   para (text (orbNumberToSpinOrbNumber nBasFun isOpenShell . fst $ i))
            ) .
        M.toList $ fi ^. imageFiles . orbImages
      Right _ ->
        chunksOf 5 .
        map (\i -> para (imageWith
                          ("", ["align-center"], [("width", "2.5cm")])
                          (i ^. _2)
                          ("Exc. " ++ (show $ i ^. _1 . _1) ++ " " ++ "Orb. " ++ (show $ i ^. _1 . _2))
                          (text $ "Exc. " ++ (show $ i ^. _1 . _1) ++ " " ++ "Orb. " ++ (show $ i ^. _1 . _2))
                        )
                   <>
                   para (text $
                           "Exc. " ++ (show $ i ^. _1 . _1) ++ "\n" ++
                           "Orb. " ++ (orbNumberToSpinOrbNumber nBasFun isOpenShell . (^. _1 . _2) $ i)
                        )
            ) .
        M.toList $ fi ^. imageFiles . natOrbImages
    -- fill single line in a transition block, which constructs an CI determinant (CIS has single
    -- pair per line, CID has two pairs per line and so on)
    ciEntry :: CIDeterminant -> String
    ciEntry ciD =
      concatMap (\x ->
        pairString x ++
        if (length (ciD ^. excitationPairs) > 1)
          then " & "
          else ""
      )  $ ciD ^. excitationPairs
    -- make nice to show string from a single orbital excitation
    pairString :: OrbitalExcitation -> String
    pairString oexc = orbString firstShow ++ arrow ++ orbString secondShow
      where
        originN = oexc ^. fromOrb . _1
        originOrb = oexc ^. fromOrb
        destinationN = oexc ^. toOrb . _1
        destinationOrb = oexc ^. toOrb
        (firstShow, secondShow, arrow) = if originN < destinationN
          then (originOrb, destinationOrb, " → ")
          else (destinationOrb, originOrb, " ← ")
        spinString :: Maybe Spin -> String
        spinString spin = case spin of
          Nothing -> ""
          Just s -> case s of
            Alpha -> "A"
            Beta  -> "B"
        orbString :: (Int, Maybe Spin) -> String
        orbString o =
          show (fst o) ++
          spinString (snd o)
    -- | Apply weight filter before printing here again.
    ciDeterminantsByWeight =
          map (V.filter (\d -> d ^. weight >= (fi ^. stateSelection . ssWeightFilter))) $
          map (^. ciWavefunction) es
    -- | Update excited states with reduced number of CI excitations. (weight filtered excited states)
    wfes =
      zipWith (\e d -> e & ciWavefunction .~ d) es ciDeterminantsByWeight
    -- CI wavefunction written by individual CI determinants (one per line), each line is a CI
    -- determinant
    manyCIEntry :: V.Vector CIDeterminant -> Blocks
    manyCIEntry ciDs = lineBlock . map text . V.toList . V.map ciEntry $ ciDs
    -- for a CI determinant give the weight in the complete CI wavefunction
    weightEntry :: CIDeterminant -> String
    weightEntry ciD = printf "%6.4F"(ciD ^. weight)
    -- line by line the weights of the CI determinants in the CI wavefunction
    manyWeightEntry :: V.Vector CIDeterminant -> Blocks
    manyWeightEntry ciDs = lineBlock . map text . V.toList . V.map weightEntry $ ciDs
    -- Look for hole image of the excited state. Give the FileInfo type, an excited state and the
    -- lens for the corresponding image type (from ImageFiles type).
    getCDDImageByType fi' eS' lens' =
      ( eS' ^. nState
      , M.lookup (eS' ^. nState) (fi' ^. imageFiles . lens')
      )
