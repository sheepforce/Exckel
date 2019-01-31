{-# LANGUAGE TemplateHaskell #-}
module Exckel.EmbedContents
( vmdTemplateScript
, exckelLogo
) where
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.FileEmbed

-- | Embed VMD template script in source code
vmdTemplateScript :: B.ByteString
vmdTemplateScript = $(embedFile "data/VMD.tcl")

-- | Exckel logo
exckelLogo :: B.ByteString
exckelLogo = $(embedFile "data/Exckel.txt")
