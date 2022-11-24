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
import           Chainweb.Api.ChainId
import           Chainweb.Api.Common
import           ChainwebData.EventDetail
import           ChainwebData.AccountDetail
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
    :<|> AccountApi

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

type TxsDetailApi = "txs"
    :> QueryParam "requestkey" RequestKey
    :> Get '[JSON] [TxDetail]

newtype EventParam = EventParam Text
deriving instance FromHttpApiData EventParam
deriving instance ToHttpApiData EventParam

newtype EventName = EventName Text
deriving instance FromHttpApiData EventName
deriving instance ToHttpApiData EventName

newtype EventModuleName = EventModuleName Text
deriving instance FromHttpApiData EventModuleName
deriving instance ToHttpApiData EventModuleName

type EventsApi = "events"
    :> LimitParam
    :> OffsetParam
    :> SearchParam
    :> QueryParam "param" EventParam
    :> QueryParam "name" EventName
    :> QueryParam "modulename" EventModuleName
    :> QueryParam "minheight" BlockHeight
    :> Get '[JSON] [EventDetail]

type AccountApi = "account"
  :> Capture "account-name" Text
  :> QueryParam "token" Text
  :> QueryParam "chain" ChainId
  :> QueryParam "fromheight" BlockHeight
  :> LimitParam
  :> OffsetParam
  :> Get '[JSON] [AccountDetail]

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
