{-# LANGUAGE TemplateHaskell #-}
module Exckel.EmbedContents
( vmdTemplateScript
, exckelLogo
, defaultDocx
, defaultODT
, spectrifyPy
) where
import qualified Data.ByteString       as B
import           Data.FileEmbed
import           System.FilePath

-- | Embed VMD template script in source code
vmdTemplateScript :: B.ByteString
vmdTemplateScript = $(embedFile $ "data" ++ [pathSeparator] ++ "VMD.tcl")

-- | Exckel logo
exckelLogo :: B.ByteString
exckelLogo = $(embedFile $ "data" ++ [pathSeparator] ++ "Exckel.txt")

-- | Reference docx document with formatting hints for pandoc
defaultDocx :: B.ByteString
defaultDocx = $(embedFile $ "data" ++ [pathSeparator] ++ "DefaultRef.docx")

-- | Reference odt document with formatting hints for pando
defaultODT :: B.ByteString
defaultODT = $(embedFile $ "data" ++ [pathSeparator] ++ "DefaultRef.odt")

-- | Spectrify python program
spectrifyPy :: B.ByteString
spectrifyPy = $(embedFile $ "spectrify" ++ [pathSeparator] ++ "spectrify.py")
