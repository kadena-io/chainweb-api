{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module ChainwebApi.Types.ChainId ( ChainId(..) ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Aeson
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)
------------------------------------------------------------------------------

newtype ChainId = ChainId { unChainId :: Int }
  deriving (Eq,Ord,Hashable)

chainIdFromText :: MonadFail m => Text -> m ChainId
chainIdFromText
  = maybe (fail "ChainId string was not an integer") (pure . ChainId)
  . readMaybe . T.unpack

instance FromJSON ChainId where
  parseJSON v = do
        withText "ChainId" chainIdFromText v
    <|> withScientific "ChainId" (pure . ChainId . round) v

instance FromJSONKey ChainId where
  fromJSONKey = FromJSONKeyTextParser chainIdFromText

instance Show ChainId where
  show (ChainId b) = show b
