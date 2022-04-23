module Chainweb.Api.Guard where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Chainweb.Api.Base64Url
------------------------------------------------------------------------------

data Guard
  = GPact PactGuard
  | GKeySet KeySet
  | GKeySetRef Text
  | GModule ModuleGuard
  | GUser UserGuard
  deriving (Eq,Ord,Show,Generic)

data KeySet = KeySet
  { _ksKeys :: Set Text
  , _ksPredFun :: Text
  } deriving (Eq,Generic,Show,Ord)

data PactGuard = PactGuard
  { _pgPactId :: Text
  , _pgName :: Text
  } deriving (Eq,Ord,Generic,Show)

data ModuleGuard = ModuleGuard
  { _mgModuleName :: Text
  , _mgName :: Text
  } deriving (Eq,Ord,Generic,Show)

data UserGuard = UserGuard
  { _ugFun :: Text
  , _ugArgs :: [Value]
  } deriving (Eq,Ord,Generic,Show)
