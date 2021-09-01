{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Chainweb.Api.PactTypes where

------------------------------------------------------------------------------
import           Data.Aeson
-- import           Data.Hashable

import           Data.Maybe (catMaybes)
import           Data.Map (Map)
import           Data.Text (Text)
-- import qualified Data.Text as T
-- import           Text.Read (readMaybe)
import           Data.Foldable (asum)
import           Data.Scientific
------------------------------------------------------------------------------
import Chainweb.Api.Base64Url
------------------------------------------------------------------------------

data PactGuard = PactGuard
  { _pactGuard_pactId :: Base64Url Text
  , _pactGuard_name   :: Text
  } deriving (Show, Eq)

instance ToJSON PactGuard where
  toJSON PactGuard{..} = object
    [ "pactId" .= toJSON _pactGuard_pactId
    , "name"   .= toJSON _pactGuard_name
    ]

instance FromJSON PactGuard where
  parseJSON = withObject "PactGuard" $ \o-> PactGuard
    <$> o .: "pactId" 
    <*> o .: "name"

--TODO: Instances are incorrect here
data ModuleGuard = ModuleGuard   -- Note: Some funky nested json that the datastructure doesn't show
  { _moduleGuard_guardName       :: Text
  , _moduleGuard_moduleName      :: Text
  , _moduleGuard_moduleNamespace :: Maybe Text
  } deriving (Show, Eq)

instance ToJSON ModuleGuard where
  toJSON ModuleGuard{..} = object
    [ "name" .= _moduleGuard_guardName
    , ("moduleName", object $ catMaybes
        [ Just $ "name" .= _moduleGuard_moduleName
        , fmap ("namespace" .=) _moduleGuard_moduleNamespace
        ]
      )
    ]
instance FromJSON ModuleGuard where
  parseJSON = withObject "ModuleGuard" $ \o -> do
    n <- o .: "name"
    mN <- o .: "moduleName"
    mN' <- mN .: "name"
    mNs <- mN .:? "namespace"
    pure $ ModuleGuard n mN' mNs


data UserGuard = UserGuard
  { _userGuard_args       :: [PactValue]
  , _userGuard_predFunc   :: Text
  } deriving (Show, Eq)

instance ToJSON UserGuard where
  toJSON UserGuard{..} = object
    [ "args" .= _userGuard_args
    , "fun"  .= _userGuard_predFunc
    ]
instance FromJSON UserGuard where
  parseJSON = withObject "UserGuard" $ \o -> UserGuard
    <$> o .: "args"
    <*> o .: "fun"

data Keyset = Keyset
  { _keyset_keys :: [Text]
  , _keyset_pred :: Text
  }
  deriving (Eq, Show)

instance FromJSON Keyset where
  parseJSON = withObject "PactKeyset" $ \o -> Keyset
    <$> o .: "keys"
    <*> o .: "pred"

instance ToJSON Keyset where
  toJSON Keyset{..} = object
    [ "keys"    .= _keyset_keys
    , "pred"    .= _keyset_pred
    ]

newtype KeysetReference = KeysetReference Text
  deriving (Show, Eq)

instance FromJSON KeysetReference where
  parseJSON = withObject "KeysetReference" $ \o -> KeysetReference
    <$> o .: "keysetref"

instance ToJSON KeysetReference where
  toJSON (KeysetReference ref) = object [ "keysetref" .= ref ]

newtype ModuleReference = ModuleReference Text
  deriving (Show, Eq)

instance FromJSON ModuleReference where
  parseJSON = withObject "ModuleReference" $ \o -> ModuleReference
    <$> o .: "refname"

instance ToJSON ModuleReference where
  toJSON (ModuleReference ref) = object [ "refname" .= ref ]
--------------------------------------------------------------
newtype PactInt = PactInt Int
  deriving (Eq, Show, Ord, Enum, Num, Real, Bounded)

instance ToJSON PactInt where
  toJSON (PactInt i) = object ["int" .= i ]

instance FromJSON PactInt where
  parseJSON = withObject "PactInt" $ \o -> PactInt <$> o .: "int"

newtype PactTime = PactTime Text
  deriving (Eq, Show)

instance ToJSON PactTime where
  toJSON (PactTime t) = object ["time" .= t ]

instance FromJSON PactTime where
  parseJSON = withObject "PactTime" $ \o -> PactTime <$> o .: "time"

data PactValue =
    PactValue_String          Text
  | PactValue_Integer         PactInt
  | PactValue_Decimal         Scientific
  | PactValue_Boolean         Bool
  | PactValue_List            [PactValue]
  | PactValue_ObjectMap       (Map Text PactValue)
  | PactValue_ModuleReference ModuleReference
  | PactValue_PactGuard       PactGuard
  | PactValue_ModuleGuard     ModuleGuard
  | PactValue_UserGuard       UserGuard
  | PactValue_Keyset          Keyset
  | PactValue_KeysetReference KeysetReference
  | PactValue_Time            PactTime
  deriving (Eq, Show)

instance FromJSON PactValue where
  parseJSON (Bool b) = pure $ PactValue_Boolean b
  parseJSON (String s) = pure $ PactValue_String s
  parseJSON (Number n) = pure $ PactValue_Decimal n
  parseJSON a@(Array _) = fmap PactValue_List $ parseJSON a
  parseJSON o = asum $ fmap (flip ($) o)
    [ fmap PactValue_Integer . parseJSON
    , fmap PactValue_ModuleReference . parseJSON
    , fmap PactValue_PactGuard . parseJSON
    , fmap PactValue_ModuleGuard . parseJSON
    , fmap PactValue_UserGuard . parseJSON
    , fmap PactValue_Keyset . parseJSON
    , fmap PactValue_KeysetReference . parseJSON
    , fmap PactValue_Time . parseJSON
    , fmap PactValue_ObjectMap . parseJSON
    ]

instance ToJSON PactValue where
  toJSON pv = case pv of
    PactValue_String s          -> toJSON s
    PactValue_Integer i         -> toJSON i
    PactValue_Decimal d         -> toJSON d
    PactValue_Boolean b         -> toJSON b
    PactValue_Time t            -> toJSON t
    PactValue_List l            -> toJSON l
    PactValue_ObjectMap m       -> toJSON m
    PactValue_ModuleReference m -> toJSON m
    PactValue_PactGuard g       -> toJSON g
    PactValue_ModuleGuard g     -> toJSON g
    PactValue_UserGuard g       -> toJSON g
    PactValue_KeysetReference k -> toJSON k
    PactValue_Keyset k          -> toJSON k
--------------------------------------------------------------
data PactEvent = PactEvent
  { _pactEvent_name       :: Text
  , _pactEvent_moduleName :: Text
  , _pactEvent_params     :: [PactValue]
  , _pactEvent_moduleHash :: Base64Url Text -- TODO a
  } deriving (Show, Eq)

instance ToJSON PactEvent where
  toJSON PactEvent{..} = object
    [ "name"        .= _pactEvent_name
    , "module"      .= _pactEvent_moduleName
    , "params"      .= _pactEvent_params
    , "moduleHash"  .= _pactEvent_moduleHash
    ]

instance FromJSON PactEvent where
  parseJSON = withObject "PactEvent" $ \o -> PactEvent
    <$> o .: "name"
    <*> o .: "module"
    <*> o .: "params"
    <*> o .: "moduleHash"

data PactContinuationDef = PactContinuationDef
  { _pactContinuationDef_name  :: Text
  , _pactContinuationDef_args  :: [ PactValue ]
  } deriving (Show, Eq)

instance FromJSON PactContinuationDef where
  parseJSON = withObject "PactContinuationDef" $ \o -> PactContinuationDef
    <$> o .: "def"
    <*> o .: "args"

data PactContinuationYield = PactContinuationYield
  { _pactContiunationYield_data           :: Value  -- Api seems inconsitent, so im punting on this for now
  , _pactContiunationYield_targetChainId  :: Text
  , _pactContiunationYield_moduleHash     :: Text 
  } deriving (Show, Eq)

instance FromJSON PactContinuationYield where
  parseJSON = withObject "PactContinuationYield" $ \o -> do
    yieldData <- o .: "data"
    prov <- o .: "provenance"
    (tcid, mh) <- case prov of
      Object subO -> (,)
        <$> subO .: "targetChainId"
        <*> subO .: "moduleHash"
      _ -> fail "PactContinuationYield: expected 'provenenance' found something else"
    return $ PactContinuationYield yieldData tcid mh

data PactContinuationResult = PactContinuationResult
  { _pactContinuationResult_pactId          :: Text
  , _pactContinuationResult_step            :: Int
  , _pactContinuationResult_stepCount       :: Int
  , _pactContinuationResult_executed        :: Bool
  , _pactContinuationResult_stepHasRollback :: Bool
  , _pactContinuationResult_continuation    :: PactContinuationDef
  , _pactContinuationResult_yield           :: PactValue
  } deriving (Show, Eq)

instance FromJSON PactContinuationResult where
  parseJSON = withObject "PactContinuationResult" $ \o -> PactContinuationResult
    <$> o .: "pactId"
    <*> o .: "step"
    <*> o .: "stepCount"
    <*> o .: "executed"
    <*> o .: "stepHasRollback"
    <*> o .: "continuation"
    <*> o .: "yield"
--------------------------------------------------------------
data PactResult =
    PactResult_Value PactValue
  | PactResult_Error PactError
  deriving (Eq, Show)

instance FromJSON PactResult where
  parseJSON = withObject "PactResult" $ \o -> do
    res :: Text <- o .: "status"
    case res of
      "success" -> fmap PactResult_Value $ o .: "data"
      "failure" -> fmap PactResult_Error $ o .: "error"
      _         -> fail "Not a valid pact result status field"

data PactErrorType =
    PactErrorType_EvalError
  | PactErrorType_ArgsError
  | PactErrorType_DbError
  | PactErrorType_TxFailure
  | PactErrorType_SyntaxError
  | PactErrorType_GasError
  deriving (Eq, Show)

-- instance ToJSON PactErrorType where
--   toJSON e = case e of
--     PactErrorType_EvalError -> String "EvalError"
--     PactErrorType_ArgsError -> String "ArgsError"
--     PactErrorType_DbError -> String "DbError"
--     PactErrorType_TxFailure -> String "TxFailure"
--     PactErrorType_SyntaxError -> String "SyntaxError"
--     PactErrorType_GasError -> String "GasError"

instance FromJSON PactErrorType where
  parseJSON = withText "PactErrorType" $ \case
    "EvalError"   -> pure PactErrorType_EvalError
    "ArgsError"   -> pure PactErrorType_ArgsError
    "DbError"     -> pure PactErrorType_DbError
    "TxFailure"   -> pure PactErrorType_TxFailure
    "SyntaxError" -> pure PactErrorType_SyntaxError
    "GasError"    -> pure PactErrorType_GasError
    _             -> fail "unknown PactErrorType"

data PactError = PactError
  { _pactError_message   :: Text
  , _pactError_callStack :: Maybe [Text]
  , _pactError_info      :: Maybe Text
  , _pactError_errorType :: Maybe PactErrorType
  } deriving (Eq, Show)

-- instance ToJSON PactError where
--   toJSON PactError{..} = object $ catMaybes
--     [ Just $ "message"   .=  _pactError_message
--     , fmap ( "callStack" .=) _pactError_callStack
--     , fmap ( "info"      .=) _pactError_info
--     , fmap ( "type"      .=) _pactError_errorType
--     ]
instance FromJSON PactError where
  parseJSON = withObject "PactError" $ \o -> PactError
    <$> o .:  "message"
    <*> o .:? "callStack"
    <*> o .:? "info"
    <*> o .:? "type"

