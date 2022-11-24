{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Chainweb.Api.ChainId
  ( ChainId(..)
  , chainIdFromText
  ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Hashable
import qualified Data.Text as T
import           GHC.Generics
import           Servant.API (FromHttpApiData,ToHttpApiData)
import           Text.Read (readMaybe)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
#endif
------------------------------------------------------------------------------

newtype ChainId = ChainId { unChainId :: Int }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Show, Hashable, ToJSONKey, FromJSONKey)
deriving instance FromHttpApiData ChainId
deriving instance ToHttpApiData ChainId

chainIdFromText :: MonadFail m => T.Text -> m ChainId
chainIdFromText
  = maybe (fail "ChainId string was not an integer") (pure . ChainId)
  . readMaybe . T.unpack

instance ToJSON ChainId where
  toJSON (ChainId c) = toJSON c

instance FromJSON ChainId where
  parseJSON v =
        withText "ChainId" chainIdFromText v
    <|> withScientific "ChainId" (pure . ChainId . round) v
