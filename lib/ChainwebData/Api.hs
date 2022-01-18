{-# LANGUAGE StandaloneDeriving #-}
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
import           ChainwebData.EventDetail
import           ChainwebData.Pagination
import           ChainwebData.TxSummary
import           ChainwebData.TxDetail
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
    :<|> TxDetailApi
    :<|> TxsDetailApi

type RecentTxsApi = "recent"
    :> Get '[JSON] [TxSummary]

type TxSearchApi = "search"
    :> LimitParam
    :> OffsetParam
    :> SearchParam
    :> Get '[JSON] [TxSummary]

newtype RequestKey = RequestKey Text
deriving instance FromHttpApiData RequestKey
deriving instance ToHttpApiData RequestKey

type TxDetailApi = "tx"
    :> QueryParam "requestkey" RequestKey
    :> Get '[JSON] TxDetail

type TxsDetailApi = "tx"
    :> QueryParam "requestkey" RequestKey
    :> Get '[JSON] [TxDetail]

newtype EventParam = EventParam Text
deriving instance FromHttpApiData EventParam
deriving instance ToHttpApiData EventParam

newtype EventName = EventName Text
deriving instance FromHttpApiData EventName
deriving instance ToHttpApiData EventName

type EventsApi = "events"
    :> LimitParam
    :> OffsetParam
    :> SearchParam
    :> QueryParam "param" EventParam
    :> QueryParam "name" EventName
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
