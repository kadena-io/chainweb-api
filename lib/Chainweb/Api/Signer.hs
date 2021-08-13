{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.Signer where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Maybe (fromMaybe, catMaybes)
import Data.Ord
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

data Signer = Signer
  { _signer_addr :: Maybe Text
  , _signer_scheme :: Maybe Text
  , _signer_pubKey :: Text
  , _signer_capList :: [SigCapability]
  } deriving (Eq,Ord,Show)

instance ToJSON Signer where
  toJSON Signer{..} = object $ catMaybes
    [ fmap ("addr" .=) _signer_addr
    , fmap ("scheme" .=) _signer_scheme
    , Just $ "pubKey" .= _signer_pubKey
    , Just $ "clist" .= _signer_capList
    ]

instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "addr"
    <*> o .:? "scheme"
    <*> o .: "pubKey"
    <*> fmap (fromMaybe []) (o .:? "clist")
