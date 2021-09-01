{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.ChainwebMeta where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock.POSIX
------------------------------------------------------------------------------

data ChainwebMeta = ChainwebMeta
  { _chainwebMeta_chainId      :: Text
  , _chainwebMeta_creationTime :: POSIXTime --UTC TIme? Or POSIX time?
  , _chainwebMeta_ttl          :: Int
  , _chainwebMeta_gasLimit     :: Int
  , _chainwebMeta_gasPrice     :: Double
  , _chainwebMeta_sender       :: Text
  } deriving (Eq,Ord,Show)

instance ToJSON ChainwebMeta where
  toJSON ChainwebMeta{..} = object
    [ "chainId"      .= _chainwebMeta_chainId
    , "creationTime" .= _chainwebMeta_creationTime
    , "ttl"          .= _chainwebMeta_ttl
    , "gasLimit"     .= _chainwebMeta_gasLimit
    , "gasPrice"     .= _chainwebMeta_gasPrice
    , "sender"       .= _chainwebMeta_sender
    ]

instance FromJSON ChainwebMeta where
  parseJSON = withObject "ChainwebMeta" $ \o -> ChainwebMeta
    <$> o .: "chainId"
    <*> o .: "creationTime"
    <*> o .: "ttl"
    <*> o .: "gasLimit"
    <*> o .: "gasPrice"
    <*> o .: "sender"
