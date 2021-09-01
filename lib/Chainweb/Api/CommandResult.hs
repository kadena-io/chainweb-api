{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.CommandResult where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
------------------------------------------------------------------------------
import Chainweb.Api.PactTypes
import Chainweb.Api.ChainwebMeta
------------------------------------------------------------------------------

data ResultMetaData = ResultMetaData
  { _resultMetaData_chainwebMeta  :: Maybe ChainwebMeta  -- only on locals
  , _resultMetaData_blockTime     :: Text
  , _resultMetaData_blockHash     :: Maybe Text   -- only on send
  , _resultMetaData_prevBlockHash :: Text
  , _resultMetaData_blockHeight   :: Int
  } deriving (Show, Eq)

instance FromJSON ResultMetaData where
  parseJSON = withObject "ResultMetaData" $ \o -> ResultMetaData
    <$> o .:? "publicMeta"
    <*> o .:  "blockTime"
    <*> o .:? "blockHash"
    <*> o .:  "prevBlockHash"
    <*> o .:  "blockHeight"

data CommandResult = CommandResult
  { _commandResult_requestKey    :: Text
  , _commandResult_result        :: PactResult
  , _commandResult_transactionID :: Maybe Int
  , _commandResult_gas           :: Int
  , _commandResult_logReference  :: Maybe Text
  , _commandResult_meta          :: Maybe ResultMetaData
  , _commandResult_continuation  :: Maybe PactContinuationResult
  , _commandResult_events        :: Maybe [PactEvent]
  } deriving (Show, Eq)

instance FromJSON CommandResult where
  parseJSON = withObject "CommandResult" $ \o -> CommandResult
    <$> o .:  "reqKey"
    <*> o .:  "result"
    <*> o .:? "txId"
    <*> o .:  "gas"
    <*> o .:? "logs"
    <*> o .:? "metaData"
    <*> o .:? "continuation"
    <*> o .:? "events"
