{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.ChainwebMeta where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock.POSIX
------------------------------------------------------------------------------
import Chainweb.Api.ChainId
import Chainweb.Api.ParsedNumbers
------------------------------------------------------------------------------

data ChainwebMeta = ChainwebMeta
  { _chainwebMeta_chainId      :: ChainId
  , _chainwebMeta_creationTime :: POSIXTime
  , _chainwebMeta_ttl          :: Int
  , _chainwebMeta_gasLimit     :: Int
  , _chainwebMeta_gasPrice     :: ParsedDecimal
  , _chainwebMeta_sender       :: Text
  } deriving (Eq,Ord,Show)

instance ToJSON ChainwebMeta where
  toJSON ChainwebMeta{..} = object
    [ "chainId" .= show (unChainId _chainwebMeta_chainId)
    , "creationTime" .= _chainwebMeta_creationTime
    , "ttl" .= _chainwebMeta_ttl
    , "gasLimit" .= _chainwebMeta_gasLimit
    , "gasPrice" .= ParsedDecimal _chainwebMeta_gasPrice
    , "sender" .= _chainwebMeta_sender
    ]

instance FromJSON ChainwebMeta where
  parseJSON = withObject "ChainwebMeta" $ \o -> ChainwebMeta
    <$> o .: "chainId"
    <*> (fromIntegral . unParsedInteger <$> o .: "creationTime")
    <*> (fromIntegral . unParsedInteger <$> o .: "ttl")
    <*> (fromIntegral . unParsedInteger <$> o .: "gasLimit")
    <*> o .: "gasPrice"
    <*> o .: "sender"
