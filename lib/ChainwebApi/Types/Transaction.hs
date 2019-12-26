{-# LANGUAGE OverloadedStrings #-}

module ChainwebApi.Types.Transaction where

------------------------------------------------------------------------------
import Data.Aeson
------------------------------------------------------------------------------
import ChainwebApi.Types.Hash
import ChainwebApi.Types.PactCommand
import ChainwebApi.Types.Sig
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
