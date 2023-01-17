{-# LANGUAGE DeriveGeneric #-}

module ChainwebData.TransferDetail where

import ChainwebData.Util
import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import Data.Time
import GHC.Generics

data TransferDetail = TransferDetail
  { _trDetail_blockHash :: Text
  , _trDetail_requestKey :: Text
  , _trDetail_chainid :: Int
  , _trDetail_height :: Int
  , _trDetail_idx :: Int
  , _trDetail_name :: Text
  , _trDetail_fromAccount :: Text
  , _trDetail_toAccount :: Text
  , _trDetail_amount :: Scientific
  , _trDetail_blockTime :: UTCTime
  } deriving (Eq, Show, Generic)

instance ToJSON TransferDetail where
    toJSON = lensyToJSON 10

instance FromJSON TransferDetail where
    parseJSON = lensyParseJSON 10
