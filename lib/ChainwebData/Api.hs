{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module ChainwebData.Api where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Proxy
import           Data.Text (Text)
import           GHC.Generics
import           Servant.API
------------------------------------------------------------------------------
import           ChainwebData.Pagination
import           ChainwebData.TxSummary
import           ChainwebData.EventDetail
------------------------------------------------------------------------------


chainwebDataApi :: Proxy ChainwebDataApi
chainwebDataApi = Proxy

type ChainwebDataApi = ("txs" :> TxApi)
                  :<|> ("stats" :> Get '[JSON] ChainwebDataStats)
                  :<|> ("coins" :> Get '[PlainText] Text)

type TxApi
    = RecentTxsApi
    :<|> TxSearchApi
    :<|> EventsApi

type RecentTxsApi = "recent"
    :> Get '[JSON] [TxSummary]

type TxSearchApi = "search"
    :> LimitParam
    :> OffsetParam
    :> SearchParam
    :> Get '[JSON] [TxSummary]

type EventParamParam = QueryParam "param" Text
type EventRequestKeyParam = QueryParam "requestkey" Text
type EventNameParam = QueryParam "name" Text
type EventIndexParam = QueryParam "index" Int

type EventsApi = "events"
    :> LimitParam
    :> OffsetParam
    :> EventParamParam
    :> EventRequestKeyParam
    :> EventNameParam
    :> EventIndexParam
    :> Get '[JSON] [EventDetail]


data ChainwebDataStats = ChainwebDataStats
  { _cds_transactionCount :: Maybe Int
  , _cds_coinsInCirculation :: Maybe Double
  } deriving (Eq,Ord,Show,Generic)

jsonOpts :: Options
jsonOpts = defaultOptions { fieldLabelModifier = drop 5 }

instance ToJSON ChainwebDataStats where
  toEncoding = genericToEncoding jsonOpts
  toJSON = genericToJSON jsonOpts
instance FromJSON ChainwebDataStats where
  parseJSON = genericParseJSON jsonOpts
