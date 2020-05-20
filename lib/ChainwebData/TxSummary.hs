{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ChainwebData.TxSummary where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
------------------------------------------------------------------------------
import           Chainweb.Api.BlockPayloadWithOutputs
import           Chainweb.Api.ChainId
import           Chainweb.Api.ChainwebMeta
import           Chainweb.Api.Common
import           Chainweb.Api.Hash
import           Chainweb.Api.Payload
import           Chainweb.Api.PactCommand
import           Chainweb.Api.Transaction
------------------------------------------------------------------------------

data TxResult
  = TxSucceeded
  | TxFailed
  | TxUnexpected -- Shouldn't happen...here for totality
  deriving (Eq,Ord,Show,Read,Enum,Generic)

instance ToJSON TxResult where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON TxResult

data TxSummary = TxSummary
  { _txSummary_chain :: Int
  , _txSummary_height :: Int
  , _txSummary_blockHash :: Text
  , _txSummary_creationTime :: UTCTime
  , _txSummary_requestKey :: Text
  , _txSummary_sender :: Text
  , _txSummary_code :: Maybe Text
  , _txSummary_continuation :: Maybe Value
  , _txSummary_result :: TxResult
  } deriving (Eq,Show,Generic)

instance ToJSON TxSummary where
    toJSON s = object
      [ "chain" .= _txSummary_chain s
      , "height" .= _txSummary_height s
      , "blockHash" .= _txSummary_blockHash s
      , "creationTime" .= _txSummary_creationTime s
      , "requestKey" .= _txSummary_requestKey s
      , "sender" .= _txSummary_sender s
      , "code" .= _txSummary_code s
      , "continuation" .= _txSummary_continuation s
      , "result" .= _txSummary_result s
      ]

instance FromJSON TxSummary where
    parseJSON = withObject "TxSummary" $ \v -> TxSummary
      <$> v .: "chain"
      <*> v .: "height"
      <*> v .: "blockHash"
      <*> v .: "creationTime"
      <*> v .: "requestKey"
      <*> v .: "sender"
      <*> v .: "code"
      <*> v .:? "continuation"
      <*> v .: "result"

mkTxSummary :: ChainId -> BlockHeight -> Hash -> Transaction -> TransactionOutput -> TxSummary
mkTxSummary (ChainId chain) height bh (Transaction th _ pc) tout =
    TxSummary chain height (hashB64U bh) t (hashB64U th) s code cont r
  where
    meta = _pactCommand_meta pc
    t = posixSecondsToUTCTime $ _chainwebMeta_creationTime meta
    s = _chainwebMeta_sender meta
    code = case _pactCommand_payload pc of
             ExecPayload e -> Just $ _exec_code e
             ContPayload _ -> Nothing
    cont = _toutContinuation tout
    r = case _toutResult tout of
          PactResult (Left _) -> TxFailed
          PactResult (Right _) -> TxSucceeded
