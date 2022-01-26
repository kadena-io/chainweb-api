{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.ChainwebMeta where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Decimal
import Data.Text (Text)
import Data.Time.Clock.POSIX
------------------------------------------------------------------------------
import Chainweb.Api.ParsedNumbers
------------------------------------------------------------------------------

data ChainwebMeta = ChainwebMeta
  { _chainwebMeta_chainId      :: Text
  , _chainwebMeta_creationTime :: POSIXTime
  , _chainwebMeta_ttl          :: Int
  , _chainwebMeta_gasLimit     :: Int
  , _chainwebMeta_gasPrice     :: Decimal
  , _chainwebMeta_sender       :: Text
  } deriving (Eq,Ord,Show)

instance FromJSON ChainwebMeta where
  parseJSON = withObject "ChainwebMeta" $ \o -> ChainwebMeta
    <$> o .: "chainId"
    <*> (fromIntegral . unParsedInteger <$> o .: "creationTime")
    <*> (fromIntegral . unParsedInteger <$> o .: "ttl")
    <*> (fromIntegral . unParsedInteger <$> o .: "gasLimit")
    <*> (unParsedDecimal <$> o .: "gasPrice")
    <*> o .: "sender"
