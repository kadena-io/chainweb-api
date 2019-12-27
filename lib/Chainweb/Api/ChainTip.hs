{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.ChainTip where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------

data ChainTip = ChainTip
  { _tipHeight :: Int
  , _tipHash   :: Text
  } deriving (Eq,Ord,Show)

instance FromJSON ChainTip where
  parseJSON = withObject "ChainTip" $ \o -> ChainTip
    <$> o .: "height"
    <*> o .: "hash"
