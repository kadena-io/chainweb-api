{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module ChainwebApi.Types.ChainId ( ChainId(..) ) where

------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.Aeson
import           Data.Hashable
import qualified Data.Text as T
import           Text.Read (readMaybe)
------------------------------------------------------------------------------

newtype ChainId = ChainId { unChainId :: Int }
  deriving stock (Eq, Ord)
  deriving newtype (Show, Hashable)

chainIdFromText :: MonadFail m => T.Text -> m ChainId
chainIdFromText
  = maybe (fail "ChainId string was not an integer") (pure . ChainId)
  . readMaybe . T.unpack

instance FromJSON ChainId where
  parseJSON v = do
        withText "ChainId" chainIdFromText v
    <|> withScientific "ChainId" (pure . ChainId . round) v

instance FromJSONKey ChainId where
  fromJSONKey = FromJSONKeyTextParser chainIdFromText
