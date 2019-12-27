{-# LANGUAGE OverloadedStrings #-}

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
  } deriving (Eq,Show)

instance FromJSON PactCommand where
  parseJSON = withObject "PactCommand" $ \o -> PactCommand
    <$> o .: "payload"
    <*> o .: "signers"
    <*> o .: "meta"
    <*> o .: "nonce"
