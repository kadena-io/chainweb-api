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

hexToBytesLE :: Text -> Either String BytesLE
hexToBytesLE t =
    if B.null invalid
      then Right $ BytesLE decoded
      else Left $ "Invalid hex string: " <> T.unpack t
  where
    (decoded, invalid) = B16.decode $ T.encodeUtf8 t

hexFromBytesLE :: BytesLE -> Text
hexFromBytesLE = T.decodeUtf8 . B16.encode . unBytesLE

leToInteger :: ByteString -> Integer
leToInteger = B.foldl' (\a b -> a * 256 + fromIntegral b) 0

instance FromJSON BytesLE where
  parseJSON = withText "BytesLE" $
    either fail (return . BytesLE . B.reverse) . decodeB64UrlNoPaddingText
