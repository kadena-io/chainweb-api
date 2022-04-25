{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.PactCommand where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------
import Chainweb.Api.ChainwebMeta
import Chainweb.Api.Payload
import Chainweb.Api.Signer
------------------------------------------------------------------------------

data PactCommand = PactCommand
  { _pactCommand_payload :: Payload
  , _pactCommand_signers :: [Signer]
  , _pactCommand_meta    :: ChainwebMeta
  , _pactCommand_nonce   :: Text
  , _pactCommand_network :: Maybe Text
  } deriving (Eq,Show)

instance ToJSON PactCommand where
  toJSON PactCommand{..} = object
    [ "payload" .= _pactCommand_payload
    , "signers" .= _pactCommand_signers
    , "meta" .= _pactCommand_meta
    , "nonce" .= _pactCommand_nonce
    , "networkId" .= _pactCommand_network
    ]

instance FromJSON PactCommand where
  parseJSON = withObject "PactCommand" $ \o -> PactCommand
    <$> o .: "payload"
    <*> o .: "signers"
    <*> o .: "meta"
    <*> o .: "nonce"
    <*> o .: "networkId"
