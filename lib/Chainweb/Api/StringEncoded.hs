{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Chainweb.Api.StringEncoded where

import qualified  Data.Aeson as A

import GHC.Generics
import Data.Typeable (Typeable, typeRep)
import Text.Read (readMaybe)
import Data.Text (unpack)
import Data.Maybe (maybeToList)

-- | Wrap "a" so that it is encoded through its Show, Read instances
newtype StringEncoded a = StringEncoded { getStringEncoded :: a }
  -- Not deriving Ord, because which Ord? Textual or original?
  -- Not deriving Num, because it's better to be explicit in the API
  deriving stock (Eq, Generic)

instance Show a => Show (StringEncoded a) where
  show = show . show . getStringEncoded

instance Read a => Read (StringEncoded a) where
  readsPrec i s = [ (StringEncoded a, t)
    | (str, t) <- readsPrec i s
    , a <- maybeToList $ readMaybe str
    ]

instance Show a => A.ToJSON (StringEncoded a) where
  toJSON = A.toJSON . show . getStringEncoded

instance (Typeable a, Read a) => A.FromJSON (StringEncoded a) where
  parseJSON = A.withText typeName $ \txt -> case readMaybe $ unpack txt of
      Just a -> return $ StringEncoded a
      Nothing -> fail $ "Invalid " ++ typeName ++ ": " ++ show txt
    where typeName = show $ typeRep ([] :: [StringEncoded a])
