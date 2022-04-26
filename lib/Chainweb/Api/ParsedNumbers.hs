{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Chainweb.Api.ParsedNumbers where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Decimal
import qualified Data.Text as T
import           GHC.Generics
import           Text.Read (readMaybe)
------------------------------------------------------------------------------

-- | JSON serialization for 'readDecimal' and public meta info;
-- accepts both a String version (parsed as a Pact decimal) or
-- a Number.
newtype ParsedDecimal = ParsedDecimal { unParsedDecimal :: Decimal }
  deriving (Eq,Ord,Num,Real,RealFrac,Fractional,Generic)

instance FromJSON ParsedDecimal where
  parseJSON (String s) =
    ParsedDecimal <$> case readMaybe (T.unpack s) of
                        Just d -> return d
                        Nothing -> fail $ "Failure parsing decimal string: " ++ show s
  parseJSON (Number n) = return $ ParsedDecimal (fromRational $ toRational n)
  parseJSON v@Object{} = ParsedDecimal <$> decoder decimalCodec v
  parseJSON v = fail $ "Failure parsing decimal: " ++ show v

instance ToJSON ParsedDecimal where
  toJSON (ParsedDecimal d) = Number $ fromRational $ toRational d

instance Show ParsedDecimal where
  show (ParsedDecimal d) = show d

-- | JSON serialization for 'readInteger' and public meta info;
-- accepts both a String version (parsed as a Pact integer),
-- a Number, or a PactValue { "int": ... } integer
newtype ParsedInteger = ParsedInteger { unParsedInteger :: Integer }
  deriving (Eq,Show,Ord,Num,Real,Enum,Integral,Generic)

instance FromJSON ParsedInteger where
  parseJSON (String s) =
    ParsedInteger <$> case readMaybe (T.unpack s) of
                        Just i -> return i
                        Nothing -> fail $ "Failure parsing integer string: " ++ show s
  parseJSON (Number n) = return $ ParsedInteger (round n)
  parseJSON v@Object{} = ParsedInteger <$> decoder integerCodec v
  parseJSON v = fail $ "Failure parsing integer: " ++ show v

instance ToJSON ParsedInteger where
  toJSON (ParsedInteger i) = Number (fromIntegral i)


-- | JSON codec pair.
data Codec a = Codec {
  encoder :: a -> Value,
  decoder :: Value -> Parser a
  }

-- | Integers encode to an object that uses Number if in reasonable JS bounds or String otherwise.
integerCodec :: Codec Integer
integerCodec = Codec encodeInteger decodeInteger
  where
    encodeInteger i
      | isSafeInteger i = object [ field .= i ]
      | otherwise = object [ field .= show i ]
    {-# INLINE encodeInteger #-}
    decodeInteger = withObject "Integer" $ \o -> do
      s <- o .: field
      case s of
        Number n -> return (round n)
        String n -> case readMaybe (T.unpack n) of
          Just i -> return i
          Nothing -> fail $ "Invalid integer value: " ++ show s
        _ -> fail $ "Invalid integer value: " ++ show s
    {-# INLINE decodeInteger #-}
    field = "int"

-- | Min, max values that Javascript doesn't mess up.
--
--   http://blog.vjeux.com/2010/javascript/javascript-max_int-number-limits.html
--   "The integer part of the Number type in Javascript is safe in [-2^53 .. 2^53] (253 = 9 007 199 254 740 992).
--    Beyond this there will be precision loss on the least significant numbers."
jsIntegerBounds :: (Integer, Integer)
jsIntegerBounds = (-9007199254740991,9007199254740991)

isSafeInteger :: Integer -> Bool
isSafeInteger i = i >= l && i <= h
  where (l,h) = jsIntegerBounds

-- | Decimals encode to a Scientific, which is encoded as an object + String
-- if mantissa precision exceeds JS.
-- TODO fromRational . toRational may not be the speediest.
decimalCodec :: Codec Decimal
decimalCodec = Codec enc dec
  where
    enc d@(Decimal _places mantissa)
      | isSafeInteger mantissa = Number $ fromRational $ toRational d
      | otherwise = object [ field .= show d ]
    {-# INLINE enc #-}
    dec (Number n) = return $ fromRational $ toRational n
    dec (Object o) = o .: field >>= \s -> case readMaybe (T.unpack s) of
      Just d -> return d
      Nothing -> fail $ "Invalid decimal value: " ++ show s
    dec v = fail $ "Invalid decimal value: " ++ show v
    {-# INLINE dec #-}
    field = "decimal"
