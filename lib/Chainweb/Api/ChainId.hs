{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Chainweb.Api.ChainId ( ChainId(..) ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Hashable
import qualified Data.Text as T
import           Text.Read (readMaybe)
#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail (MonadFail)
#endif
------------------------------------------------------------------------------

newtype ChainId = ChainId { unChainId :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Hashable)

chainIdFromText :: MonadFail m => T.Text -> m ChainId
chainIdFromText
  = maybe (fail "ChainId string was not an integer") (pure . ChainId)
  . readMaybe . T.unpack

instance ToJSON ChainId where
  toJSON = toJSON

instance FromJSON ChainId where
  parseJSON v =
        withText "ChainId" chainIdFromText v
    <|> withScientific "ChainId" (pure . ChainId . round) v

--TODO: Is this right?
instance ToJSONKey ChainId where
  toJSONKey = toJSONKey

instance FromJSONKey ChainId where
  fromJSONKey = FromJSONKeyTextParser chainIdFromText
