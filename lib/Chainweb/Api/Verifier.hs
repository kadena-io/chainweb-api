{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Chainweb.Api.Verifier where

------------------------------------------------------------------------------
import Chainweb.Api.SigCapability
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Text (Text)
------------------------------------------------------------------------------

data Verifier = Verifier
  { _verifier_name :: Maybe Text
  , _verifier_proof :: Maybe Text
  , _verifier_capList :: [SigCapability]
  } deriving (Eq, Show)

instance Ord Verifier where
  compare = comparing _verifier_name

instance ToJSON Verifier where
  toJSON Verifier{..} = object $ catMaybes
    [ fmap ("name" .=) _verifier_name
    , fmap ("proof" .=) _verifier_proof
    , Just $ "clist" .= _verifier_capList
    ]

instance FromJSON Verifier where
  parseJSON = withObject "Verifier" $ \o -> Verifier
    <$> o .:? "name"
    <*> o .:? "proof"
    <*> o .: "clist"
