{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.BlockHeaderTx where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------
import Chainweb.Api.BlockHeader
import Chainweb.Api.BlockPayload
------------------------------------------------------------------------------

data BlockHeaderTx = BlockHeaderTx
  { _blockHeaderTx_header  :: BlockHeader
  , _blockHeaderTx_txCount :: Maybe Int
  , _blockHeaderTx_powHash :: Maybe Text
  , _blockHeaderTx_target  :: Maybe Text
  , _blockHeaderTx_payload :: Maybe BlockPayload
  } deriving (Eq,Show)

instance FromJSON BlockHeaderTx where
  parseJSON = withObject "BlockHeaderTx" $ \o -> BlockHeaderTx
    <$> o .: "header"
    <*> o .: "txCount"
    <*> o .:? "powHash"
    <*> o .:? "target"
    <*> o .:? "payload"
