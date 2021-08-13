module Chainweb.Api.Hash where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
------------------------------------------------------------------------------
import           Chainweb.Api.Base64Url
------------------------------------------------------------------------------

newtype Hash = Hash { unHash :: ByteString }
  deriving (Eq,Ord,Show,Read)

instance ToJSON Hash where
  toJSON hash = String $ hashB64U hash

instance FromJSON Hash where
  parseJSON (String t) =
    either (\e -> fail $ "Base64Url parse failed: " <> e) (return . Hash) $
      decodeB64UrlNoPaddingText t
  parseJSON invalid = typeMismatch "String" invalid

hashHex :: Hash -> Text
hashHex = T.decodeUtf8 . B16.encode . unHash

hashB64U :: Hash -> Text
hashB64U = encodeB64UrlNoPaddingText . unHash

