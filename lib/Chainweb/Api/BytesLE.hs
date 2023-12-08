{-# LANGUAGE CPP #-}

module Chainweb.Api.BytesLE where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
------------------------------------------------------------------------------
import           Chainweb.Api.Base64Url
------------------------------------------------------------------------------

newtype BytesLE = BytesLE
  { unBytesLE :: ByteString
  } deriving (Eq,Ord,Show)

#if MIN_VERSION_base16_bytestring(1,0,0)
-- Newer version of base16-bytestring
hexToBytesLE :: Text -> Either String BytesLE
hexToBytesLE t = case B16.decode $ T.encodeUtf8 t of
  Right decoded -> Right $ BytesLE decoded
  Left _ -> Left $ "Invalid hex string: " <> T.unpack t
#else
-- Older version of base16-bytestring
hexToBytesLE :: Text -> Either String BytesLE
hexToBytesLE t =
    if B.null invalid
      then Right $ BytesLE decoded
      else Left $ "Invalid hex string: " <> T.unpack t
  where
    (decoded, invalid) = B16.decode $ T.encodeUtf8 t
#endif

hexFromBytesLE :: BytesLE -> Text
hexFromBytesLE = T.decodeUtf8 . B16.encode . unBytesLE

leToInteger :: ByteString -> Integer
leToInteger = B.foldr (\b a -> a * 256 + fromIntegral b) 0

instance FromJSON BytesLE where
  parseJSON = withText "BytesLE" $
    either fail (return . BytesLE) . decodeB64UrlNoPaddingText
