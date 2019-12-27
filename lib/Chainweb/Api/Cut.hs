{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.Cut where

------------------------------------------------------------------------------
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
------------------------------------------------------------------------------
import Chainweb.Api.ChainId
import Chainweb.Api.ChainTip
import Chainweb.Api.Common
------------------------------------------------------------------------------

data Cut = Cut
  { _cutId       :: Text
  , _cutHeight   :: BlockHeight
  , _cutWeight   :: Text
  , _cutInstance :: Text
  , _cutChains   :: HashMap ChainId ChainTip
  } deriving (Eq,Ord,Show)

instance FromJSON Cut where
  parseJSON = withObject "Cut" $ \o -> Cut
    <$> o .: "id"
    <*> o .: "height"
    <*> o .: "weight"
    <*> o .: "instance"
    <*> o .: "hashes"
    -- <*> (HM.fromList . map (first fromText) . HM.toList <$> (o .: "hashes"))
