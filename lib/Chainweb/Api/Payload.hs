{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.Payload where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
------------------------------------------------------------------------------

data Exec = Exec
  { _exec_code :: Text
  , _exec_data :: Maybe Value
  } deriving (Eq,Show)

instance ToJSON Exec where
  toJSON Exec{..} = object
    [ "code" .= _exec_code
    , "data" .= _exec_data
    ]

instance FromJSON Exec where
  parseJSON = withObject "Exec" $ \o -> Exec
    <$> o .: "code"
    <*> o .: "data"

data Cont = Cont
  { _cont_pactId   :: Text
  , _cont_rollback :: Bool
  , _cont_step     :: Int
  , _cont_data     :: Value
  , _cont_proof    :: Maybe Text
  } deriving (Eq,Show)

instance ToJSON Cont where
  toJSON Cont{..} = object
    [ "pactId" .= _cont_pactId
    , "rollback" .= _cont_rollback
    , "step" .= _cont_step
    , "data" .= _cont_data
    , "proof" .= _cont_proof
    ]

instance FromJSON Cont where
  parseJSON = withObject "Cont" $ \o -> Cont
    <$> o .: "pactId"
    <*> o .: "rollback"
    <*> o .: "step"
    <*> o .: "data"
    <*> o .: "proof"

data Payload = ExecPayload Exec | ContPayload Cont
  deriving (Eq,Show)

instance ToJSON Payload where
  toJSON (ExecPayload exec) = Object $ "exec" .= toJSON exec
  toJSON (ContPayload cont) = Object $ "cont" .= toJSON cont

instance FromJSON Payload where
  parseJSON = withObject "Payload" $ \o -> case HM.lookup "exec" o of
    Nothing -> case HM.lookup "cont" o of
                 Nothing -> fail "Payload must be exec or cont"
                 Just v  -> ContPayload <$> parseJSON v
    Just v -> ExecPayload <$> parseJSON v

payloadCode :: Payload -> Text
payloadCode (ExecPayload e) = _exec_code e
payloadCode (ContPayload c) = _cont_pactId c
