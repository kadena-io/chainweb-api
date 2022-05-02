{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.Transaction where

------------------------------------------------------------------------------
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Data.Text.Encoding
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

mkTransaction :: PactCommand -> [Sig] -> Transaction
mkTransaction pc sigs =
    Transaction (Hash h) sigs pc (decodeUtf8 cmdBytes)
  where
    cmdBytes = BL.toStrict $ encode pc

    -- This function only returns Left when one of the first two args is
    -- invalid. In this case we're supplying them both as constants, so the
    -- incomplete pattern match is safe here.
    Right h = blake2b 32 mempty cmdBytes

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
