{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Chainweb.Api.SigCapability where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Ord (comparing)
import Data.Text (Text)
------------------------------------------------------------------------------

data SigCapability = SigCapability
  { _scName :: Text
  , _scArgs :: [Value]
  } deriving (Eq,Show)

instance Ord SigCapability where
  compare = comparing _scName

instance ToJSON SigCapability where
  toJSON SigCapability{..} = object
    [ "name" .= _scName
    , "args" .= _scArgs
    ]

instance FromJSON SigCapability where
  parseJSON = withObject "SigCapability" $ \o -> SigCapability
    <$> o .: "name"
    <*> o .: "args"
