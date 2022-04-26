{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.Transaction where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------
import Chainweb.Api.Hash
import Chainweb.Api.PactCommand
import Chainweb.Api.Sig
import Crypto.Hash.Blake2Native
------------------------------------------------------------------------------

data Transaction = Transaction
  { _transaction_hash :: Hash
  , _transaction_sigs :: [Sig]
  , _transaction_cmd  :: PactCommand
  , _transaction_cmdStr :: Text
  } deriving (Eq,Show)

mkTransaction :: PactCommand -> [Sig] -> Either String Transaction
mkTransaction pc sigs = do
    h <- blake2b 32 mempty cmdBytes
    pure $ Transaction (Hash h) sigs pc
  where
    cmdBytes = BSL.toStrict $ encode pc

instance ToJSON Transaction where
  toJSON Transaction{..} = object
    [ "hash" .= _transaction_hash
    , "sigs" .= _transaction_sigs
    , "cmd" .= _transaction_cmdStr
    ]

instance FromJSON Transaction where
  parseJSON = withObject "Transaction" $ \o -> do
    cmdRaw <- o .: "cmd"
    Transaction
      <$> o .: "hash"
      <*> o .: "sigs"
      <*> (withEmbeddedJSON "Embedded Cmd" parseJSON (String cmdRaw))
      <*> pure cmdRaw
