{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.RespItems where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------

data RespItems a = RespItems
  { _respItems_next  :: Maybe Text
  , _respItems_items :: [a]
  , _respItems_limit :: Int
  } deriving (Eq,Ord,Show)

instance FromJSON a => FromJSON (RespItems a) where
  parseJSON = withObject "RespItems" $ \o -> RespItems
    <$> o .: "next"
    <*> o .: "items"
    <*> o .: "limit"
