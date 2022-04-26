{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.LocalResult where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
------------------------------------------------------------------------------

-- | Skipped the publicMeta, continuation, and txId fields because they didn't
-- seem necessary at this point.  Can add them later if necessary.
data LocalResult a = LocalResult
  { _lr_status :: Text
  , _lr_data :: a
  , _lr_gas :: Int
  , _lr_reqKey :: Text
  , _lr_logs :: Text
  , _lr_blockTime :: Integer
  , _lr_prevBlockHash :: Text
  , _lr_blockHeight :: Int
  } deriving (Eq, Ord, Show, Functor)

instance FromJSON a => FromJSON (LocalResult a) where
  parseJSON = withObject "PactResult" $ \o -> do
    r <- o .: "result"
    m <- o .: "metaData"
    LocalResult
      <$> r .: "status"
      <*> r .: "data"
      <*> o .: "gas"
      <*> o .: "reqKey"
      <*> o .: "logs"
      <*> m .: "blockTime"
      <*> m .: "prevBlockHash"
      <*> m .: "blockHeight"

