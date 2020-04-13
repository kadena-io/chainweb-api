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
  , _txSummary_result :: TxResult
  } deriving (Eq,Ord,Show,Generic)

instance ToJSON TxSummary where
    toJSON s = object
      [ "chain" .= _txSummary_chain s
      , "height" .= _txSummary_height s
      , "blockHash" .= _txSummary_blockHash s
      , "creationTime" .= _txSummary_creationTime s
      , "requestKey" .= _txSummary_requestKey s
      , "sender" .= _txSummary_sender s
      , "code" .= _txSummary_code s
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
      <*> v .: "result"

mkTxSummary :: ChainId -> BlockHeight -> Hash -> Transaction -> TxSummary
mkTxSummary (ChainId chain) height (Hash bh) (Transaction h _ pc) =
    TxSummary chain height (T.decodeUtf8 bh) t (T.decodeUtf8 $ unHash h) s c r
  where
    meta = _pactCommand_meta pc
    t = posixSecondsToUTCTime $ _chainwebMeta_creationTime meta
    s = _chainwebMeta_sender meta
    c = case _pactCommand_payload pc of
          ExecPayload e -> Just $ _exec_code e
          ContPayload _ -> Nothing
    r = TxUnexpected
