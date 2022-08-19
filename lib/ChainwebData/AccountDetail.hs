{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module ChainwebData.AccountDetail where

import ChainwebData.Util
import Data.Aeson
import Data.Decimal (Decimal)
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
  , _acDetail_amount :: Decimal
  } deriving (Eq, Show, Generic)


instance ToJSON Decimal where
  toJSON d = Number $ fromRational $ toRational d

instance FromJSON Decimal where
  parseJSON = withScientific "Decimal" (pure . fromRational . toRational)

instance ToJSON AccountDetail where
    toJSON = lensyToJSON 10

instance FromJSON AccountDetail where
    parseJSON = lensyParseJSON 10
