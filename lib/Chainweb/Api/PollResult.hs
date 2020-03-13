{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.PollResult where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------
import Chainweb.Api.ChainwebMeta
import Chainweb.Api.Payload
import Chainweb.Api.Signer
------------------------------------------------------------------------------

data PollResult = PollResult {
    _cr_reqKey :: Text
  , _cr_txId :: Word64
  , _cr_result :: !PactResult
  , _cr_gas :: Int
  , _cr_logs :: Text
  , _cr_continuation :: Value
  , _cr_metaData :: Maybe Value
  } deriving (Eq,Show,Generic)

instance FromJSON PollResult where
  parseJSON = withObject "PollResult" $ \o -> PollResult
    <$> fmap (/1000000.0) (o .: "creationTime")
    <*> o .: "parent"
    <*> o .: "height"
    <*> o .: "hash"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> fmap (/1000000.0) (o .: "epochStart")
    <*> o .: "adjacents"
    <*> (o .: "payloadHash")
    <*> o .: "chainwebVersion"
    <*> o .: "target"
    <*> o .: "featureFlags"
    <*> (fromText =<< (o .: "nonce"))
