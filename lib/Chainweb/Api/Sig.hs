{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.Sig where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Text (Text)
------------------------------------------------------------------------------

newtype Sig = Sig { unSig :: Text }
  deriving (Eq,Show)

--TODO: Must be b16 according to pact spec
instance ToJSON Sig where
  toJSON s = object [ "sig" .= unSig s]

instance FromJSON Sig where
  parseJSON = withObject "Sig" $ \o -> Sig
    <$> o .: "sig"
