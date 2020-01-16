{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chainweb.Api.BlockPayloadWithOutputs where

------------------------------------------------------------------------------
import Control.Applicative
import Data.Aeson
import Data.Bifunctor
------------------------------------------------------------------------------
import Chainweb.Api.Base64Url
import Chainweb.Api.Hash
import Chainweb.Api.MinerData
import Chainweb.Api.Transaction
------------------------------------------------------------------------------

newtype PactResult = PactResult (Either Value Value) deriving (Eq, Show)

instance FromJSON PactResult where
    parseJSON = withObject "PactResult" $ \o -> (PactResult . Left <$> o .: "error") <|> (PactResult . Right <$> o .: "data")

data TransactionOutput = TransactionOutput
    { _toutGas :: Int
    , _toutResult :: PactResult
    , _toutReqKey :: Hash
    , _toutLogs :: Maybe Hash
    , _toutMetaData :: Maybe Value
    , _toutContinuation :: Maybe Value
    , _toutTxId :: Int
    } deriving (Eq, Show)

instance FromJSON TransactionOutput where
  parseJSON =
        withObject "TransactionOutput" $ \o -> TransactionOutput
            <$> o .: "gas"
            <*> o .: "result"
            <*> o .: "reqKey"
            <*> o .: "logs"
            <*> o .: "metaData"
            <*> o .: "continuation"
            <*> o .: "txId"

newtype Coinbase =
  Coinbase TransactionOutput
    deriving newtype (FromJSON, Eq, Show)

data BlockPayloadWithOutputs = BlockPayloadWithOutputs
    { _blockPayloadWithOutputs_minerData :: MinerData
    , _blockPayloadWithOutputs_transactionsHash :: Hash
    , _blockPayloadWithOutputs_outputsHash :: Hash
    , _blockPayloadWithOutputs_payloadHash :: Hash
    , _blockPayloadWithOutputs_transactionsWithOutputs :: [(Transaction, TransactionOutput)]
    , _blockPayloadWithOutputs_coinbase :: Coinbase
    } deriving (Eq,Show)

instance FromJSON BlockPayloadWithOutputs where
    parseJSON =
        withObject "BlockPayloadWithOutputs" $ \o -> BlockPayloadWithOutputs
            <$> (fromBase64Url <$> o .: "minerData")
            <*> o .: "transactionsHash"
            <*> o .: "outputsHash"
            <*> o .: "payloadHash"
            <*> (fmap (bimap fromBase64Url fromBase64Url) <$> o .: "transactions")
            <*> (fromBase64Url <$> o .: "coinbase")
