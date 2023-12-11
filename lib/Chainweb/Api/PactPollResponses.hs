{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Chainweb.Api.PactPollResponses where

------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.Text (Text)
import           Data.Word
------------------------------------------------------------------------------

newtype RequestKey = RequestKey { unRequestKey :: Text }
  deriving (Eq, Ord, Show, Hashable, FromJSON, FromJSONKey)

newtype TxId = TxId Word64
  deriving (Eq, Ord, Show, FromJSON)

newtype PactResult = PactResult { unPactResult :: Either Value Value }
  deriving (Eq, Show)


instance FromJSON PactResult where
  parseJSON (Object o) = PactResult <$> (Right <$> o .: "data" <|> Left <$> o .: "error")
  parseJSON p = fail $ "Invalid PactResult: " ++ show p

newtype Gas = Gas Int64
  deriving (Eq, Ord, Show, FromJSON)

data CommandResult l = CommandResult
  { _crReqKey :: !RequestKey
  , _crTxId :: !(Maybe TxId)
  , _crResult :: !PactResult
  , _crGas :: !Gas
  , _crLogs :: !(Maybe l)
  , _crContinuation :: !(Maybe Value) -- just accept the json?
  , _crMetaData :: !(Maybe Value)
  , _crEvents :: [Value] -- just accept the json?
  } deriving (Eq, Show)

instance FromJSON l => FromJSON (CommandResult l) where
  parseJSON = withObject "CommandResult" $ \o -> CommandResult
      <$> o .: "reqKey"
      <*> o .:? "txId"
      <*> o .: "result"
      <*> o .: "gas"
      <*> o .:? "logs"
      <*> o .:? "continuation"
      <*> o .:? "metaData"
      <*> (events <$> o .: "events")
    where
      events Nothing = []
      events (Just es) = es

newtype PactHash = PactHash { unHash :: Text }
  deriving (Eq, Ord, Show, FromJSON)

newtype PollResponses = PollResponses
  { _prResults :: HM.HashMap RequestKey (CommandResult PactHash)
  } deriving (Eq, Show)

instance FromJSON PollResponses where
  parseJSON v = PollResponses <$> withObject "PollResponses" (\_ -> parseJSON v) v
