{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.BlockPayload where

------------------------------------------------------------------------------
import Data.Aeson
------------------------------------------------------------------------------
import Chainweb.Api.Base64Url
import Chainweb.Api.Hash
import Chainweb.Api.MinerData
import Chainweb.Api.Transaction
------------------------------------------------------------------------------

data BlockPayload = BlockPayload
  { _blockPayload_minerData        :: MinerData
  , _blockPayload_transactionsHash :: Hash
  , _blockPayload_outputsHash      :: Hash
  , _blockPayload_payloadHash      :: Hash
  , _blockPayload_transactions     :: [Transaction]
  } deriving (Eq,Show)

instance FromJSON BlockPayload where
  parseJSON = withObject "BlockPayload" $ \o -> BlockPayload
    <$> (fromBase64Url <$> o .: "minerData")
    <*> o .: "transactionsHash"
    <*> o .: "outputsHash"
    <*> o .: "payloadHash"
    <*> (fmap fromBase64Url <$> o .: "transactions")
