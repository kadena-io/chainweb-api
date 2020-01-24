{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.Signer where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Ord
import Data.Text (Text)
------------------------------------------------------------------------------

data SigCapability = SigCapability
  { _scName :: Text
  , _scArgs :: [Value]
  } deriving (Eq,Show)

instance Ord SigCapability where
  compare = comparing _scName

instance FromJSON SigCapability where
  parseJSON = withObject "SigCapability" $ \o -> SigCapability
    <$> o .: "name"
    <*> o .: "args"

data Signer = Signer
  { _signer_addr   :: Maybe Text
  , _signer_scheme :: Maybe Text
  , _signer_pubKey :: Text
  , _signer_capList :: [SigCapability]
  } deriving (Eq,Ord,Show)

instance FromJSON Signer where
  parseJSON = withObject "Signer" $ \o -> Signer
    <$> o .:? "addr"
    <*> o .:? "scheme"
    <*> o .: "pubKey"
    <*> fmap (maybe [] id) (o .:? "clist")
