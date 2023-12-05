{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Chainweb.Api.Guard where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Maybe
import           Data.Set (Set)
import           Data.String (IsString)
import           Data.Text (Text)
------------------------------------------------------------------------------

data Guard
  = GPact PactGuard
  | GKeySet KeySet
  | GKeySetRef Text
  | GModule ModuleGuard
  | GUser UserGuard
  deriving (Eq,Show)

keyNamef :: IsString s => s
keyNamef = "keysetref"

instance ToJSON Guard where
  toJSON = \case
    (GKeySet k) -> toJSON k
    (GKeySetRef n) -> object [ keyNamef .= n ]
    (GPact g) -> toJSON g
    (GModule g) -> toJSON g
    (GUser g) -> toJSON g

instance FromJSON Guard where
  parseJSON v =
    (GKeySet <$> parseJSON v) <|>
    (withObject "KeySetRef" $ \o ->
        GKeySetRef <$> o .: keyNamef) v <|>
    (GPact <$> parseJSON v) <|>
    (GModule <$> parseJSON v) <|>
    (GUser <$> parseJSON v)

data KeySet = KeySet
  { _ksKeys :: Set Text
  , _ksPredFun :: Text
  } deriving (Eq,Show)

-- | allow `{ "keys": [...], "pred": "..." }`, `{ "keys": [...] }`, and just `[...]`,
-- | the latter cases defaulting to "keys-all"
instance FromJSON KeySet where
    parseJSON v = withObject "KeySet" (\o ->
                    KeySet <$> o .: "keys" <*>
                    (fromMaybe defPred <$> o .:? "pred")) v <|>
                  (KeySet <$> parseJSON v <*> pure defPred)
      where defPred = "keys-all"
instance ToJSON KeySet where
    toJSON (KeySet k f) = object
      [ "keys" .= k
      , "pred" .= f
      ]

data PactGuard = PactGuard
  { _pgPactId :: Text
  , _pgName :: Text
  } deriving (Eq,Show)

instance ToJSON PactGuard where
    toJSON (PactGuard pid nm) = object
      [ "pactId" .= pid
      , "name" .= nm
      ]

instance FromJSON PactGuard where
  parseJSON = withObject "PactGuard" $ \o -> PactGuard
    <$> o .: "pactId"
    <*> o .: "name"

data ModuleGuard = ModuleGuard
  { _mgModuleName :: Text
  , _mgName :: Text
  } deriving (Eq,Show)

instance ToJSON ModuleGuard where
    toJSON (ModuleGuard mn n) = object
      [ "moduleName" .= mn
      , "name" .= n
      ]

instance FromJSON ModuleGuard where
  parseJSON = withObject "ModuleGuard" $ \o -> ModuleGuard
    <$> o .: "moduleName"
    <*> o .: "name"

data UserGuard = UserGuard
  { _ugFun :: Text
  , _ugArgs :: [Value]
  } deriving (Eq,Show)

instance ToJSON UserGuard where
    toJSON (UserGuard fun args) = object
      [ "fun" .= fun
      , "args" .= args
      ]

instance FromJSON UserGuard where
  parseJSON = withObject "UserGuard" $ \o -> UserGuard
    <$> o .: "fun"
    <*> o .: "args"
