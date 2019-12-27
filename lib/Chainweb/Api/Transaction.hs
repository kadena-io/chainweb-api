{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.Transaction where

------------------------------------------------------------------------------
import Data.Aeson
------------------------------------------------------------------------------
import Chainweb.Api.Hash
import Chainweb.Api.PactCommand
import Chainweb.Api.Sig
------------------------------------------------------------------------------

data Transaction = Transaction
  { _transaction_hash :: Hash
  , _transaction_sigs :: [Sig]
  , _transaction_cmd  :: PactCommand
  } deriving (Eq,Show)

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> Transaction
    <$> o .: "hash"
    <*> o .: "sigs"
    <*> (withEmbeddedJSON "sig-embedded" parseJSON =<< (o .: "cmd"))
