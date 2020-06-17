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
------------------------------------------------------------------------------


chainwebDataApi :: Proxy ChainwebDataApi
chainwebDataApi = Proxy

type ChainwebDataApi = ("txs" :> TxApi)
                  :<|> ("stats" :> Get '[JSON] ChainwebDataStats)
                  :<|> ("coins" :> Get '[PlainText] Text)

type TxApi = RecentTxsApi :<|> TxSearchApi

type RecentTxsApi = "recent" :> Get '[JSON] [TxSummary]
type TxSearchApi = "search" :> LimitParam :> OffsetParam :> SearchParam :> Get '[JSON] [TxSummary]

data ChainwebDataStats = ChainwebDataStats
  { _cds_transactionCount :: Maybe Int
  , _cds_coinsInCirculation :: Maybe Double
  , _cds_maxPossibleCoins :: Double
  } deriving (Eq,Ord,Show,Generic)

jsonOpts = defaultOptions { fieldLabelModifier = drop 5 }

instance ToJSON ChainwebDataStats where
  toEncoding = genericToEncoding jsonOpts
  toJSON = genericToJSON jsonOpts
instance FromJSON ChainwebDataStats where
  parseJSON = genericParseJSON jsonOpts
