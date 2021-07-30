{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.Base64Url where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
------------------------------------------------------------------------------

newtype Base64Url a = Base64Url { fromBase64Url :: a }
  deriving (Eq,Ord,Show)

instance FromJSON a => FromJSON (Base64Url a) where
  parseJSON = withText "Base64Url" $ \t ->
    case decodeB64UrlNoPaddingText t of
      Left e -> fail e
      Right bs -> either fail (pure . Base64Url) $
                    eitherDecode $ BL.fromStrict bs

decodeB64UrlNoPaddingText :: Text -> Either String ByteString
decodeB64UrlNoPaddingText = B64U.decode . T.encodeUtf8 . pad
  where
    pad t = let s = T.length t `mod` 4 in t <> T.replicate ((4 - s) `mod` 4) "="

-- Copied from chainweb-node

-- | Encode a binary value to a textual base64-url without padding
-- representation.
--
encodeB64UrlNoPaddingText :: ByteString -> T.Text
encodeB64UrlNoPaddingText = T.dropWhileEnd (== '=') . T.decodeUtf8 . B64U.encode

