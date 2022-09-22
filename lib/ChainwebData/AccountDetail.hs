{-# LANGUAGE DeriveGeneric #-}

module ChainwebData.AccountDetail where

import Chainweb.Api.ParsedNumbers
import ChainwebData.Util
import Data.Aeson
import Data.Scientific
import Data.Text (Text)
import GHC.Generics

data AccountDetail = AccountDetail
  { _acDetail_blockHash :: Text
  , _acDetail_requestKey :: Text
  , _acDetail_chainid :: Int
  , _acDetail_height :: Int
  , _acDetail_idx :: Int
  , _acDetail_name :: Text
  , _acDetail_fromAccount :: Text
  , _acDetail_toAccount :: Text
  , _acDetail_amount :: Scientific
  } deriving (Eq, Show, Generic)

instance ToJSON AccountDetail where
    toJSON = lensyToJSON 10

instance FromJSON AccountDetail where
    parseJSON = lensyParseJSON 10
