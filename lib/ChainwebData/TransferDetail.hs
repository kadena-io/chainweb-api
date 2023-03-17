{-# LANGUAGE DeriveGeneric #-}

module ChainwebData.TransferDetail where

import Chainweb.Api.StringEncoded
import ChainwebData.Util
import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import Data.Time
import GHC.Generics

data TransferDetail = TransferDetail
  { _trDetail_blockHash :: Text
  , _trDetail_requestKey :: Text
  , _trDetail_chain :: Int
  , _trDetail_height :: Int
  , _trDetail_idx :: Int
  , _trDetail_token :: Text
  , _trDetail_fromAccount :: Text
  , _trDetail_toAccount :: Text
  , _trDetail_xchainAccount :: Maybe Text
  , _trDetail_xchainId :: Maybe Int
  , _trDetail_amount :: StringEncoded Scientific
  , _trDetail_blockTime :: UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON TransferDetail where
    toJSON = lensyToJSON 10

instance FromJSON TransferDetail where
    parseJSON = lensyParseJSON 10
