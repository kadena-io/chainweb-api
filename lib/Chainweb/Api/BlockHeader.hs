{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Chainweb.Api.BlockHeader where

------------------------------------------------------------------------------
import Control.Monad
import Crypto.Hash.BLAKE2.BLAKE2s (hash)
import Data.Aeson
import qualified Data.ByteString as B (take)
import qualified Data.Map as M (Map, fromList, assocs)
import Data.Readable
import Data.Serialize.Get
import Data.Serialize.Put
import Data.Text (Text, unpack)
import Data.Time.Clock.POSIX
import Data.Word
------------------------------------------------------------------------------
import Chainweb.Api.BytesLE
import Chainweb.Api.ChainId
import Chainweb.Api.Common
import Chainweb.Api.Hash
------------------------------------------------------------------------------

data BlockHeader = BlockHeader
  { _blockHeader_creationTime :: POSIXTime
  , _blockHeader_parent       :: Hash
  , _blockHeader_height       :: BlockHeight
  , _blockHeader_hash         :: Hash
  , _blockHeader_chainId      :: ChainId
  , _blockHeader_weight       :: BytesLE
  , _blockHeader_epochStart   :: POSIXTime
  , _blockHeader_neighbors    :: M.Map ChainId Hash
  , _blockHeader_payloadHash  :: Hash
  , _blockHeader_chainwebVer  :: Text
  , _blockHeader_target       :: BytesLE
  , _blockHeader_flags        :: Word64
  , _blockHeader_nonce        :: Word64
  } deriving (Eq,Ord,Show)

blockDifficulty :: BlockHeader -> Double
blockDifficulty =
  fromIntegral . targetToDifficulty . leToInteger . unBytesLE . _blockHeader_target

targetToDifficulty :: Integer -> Integer
targetToDifficulty target = (2 ^ (256 :: Int) - 1) `div` target

instance FromJSON BlockHeader where
  parseJSON = withObject "BlockHeader" $ \o -> BlockHeader
    <$> fmap (/1000000.0) (o .: "creationTime")
    <*> o .: "parent"
    <*> o .: "height"
    <*> o .: "hash"
    <*> o .: "chainId"
    <*> o .: "weight"
    <*> fmap (/1000000.0) (o .: "epochStart")
    <*> o .: "adjacents"
    <*> (o .: "payloadHash")
    <*> o .: "chainwebVersion"
    <*> o .: "target"
    <*> o .: "flags"
    <*> (fromText =<< (o .: "nonce"))

powHash :: BlockHeader -> Hash
powHash = Hash . hash 32 mempty . B.take 386 . runPut . encodeBlockHeader
{-# INLINE powHash #-}

-- -------------------------------------------------------------------------- --
-- Binary Encoding for BlockHeader

-- | Encode a BlockHeader
--
encodeBlockHeader :: Putter BlockHeader
encodeBlockHeader h = do
  putWord64le $ _blockHeader_nonce h
  encodePosixTime $ _blockHeader_creationTime h
  encodeHash $ _blockHeader_parent h
  encodeAdjacents $ _blockHeader_neighbors h
  encodeHashNat $ _blockHeader_target h
  encodeHash $ _blockHeader_payloadHash h
  encodeChainId $ _blockHeader_chainId h
  encodeHashNat $ _blockHeader_weight h
  putWord32le . fromIntegral $ _blockHeader_height h
  encodeChainwebVersion $ _blockHeader_chainwebVer h
  encodePosixTime $ _blockHeader_epochStart h
  putWord64le $ _blockHeader_flags h
  encodeHash $ _blockHeader_hash h

encodePosixTime :: Putter POSIXTime
encodePosixTime = putWord64le
  . round -- This is unfortunate. We should consider changing the type of _blockHeader_creationTime to be integral
  . (* 1000000)
  . realToFrac @_ @Double
{-# INLINE encodePosixTime #-}

encodeHash :: Putter Hash
encodeHash = putByteString . unHash
{-# INLINE encodeHash #-}

encodeAdjacents :: Putter (M.Map ChainId Hash)
encodeAdjacents m = do
  putWord16le (fromIntegral $ length m)
  mapM_ (\(cid, h) -> encodeChainId cid >> encodeHash h) (M.assocs m)
{-# INLINE encodeAdjacents #-}

encodeHashNat :: Putter BytesLE
encodeHashNat = putByteString . unBytesLE
{-# INLINE encodeHashNat #-}

encodeChainId :: Putter ChainId
encodeChainId = putWord32le . fromIntegral . unChainId
{-# INLINE encodeChainId #-}

encodeChainwebVersion :: Putter Text
encodeChainwebVersion "development" = putWord32le 0x01
encodeChainwebVersion "mainnet01" = putWord32le 0x05
encodeChainwebVersion "testnet04" = putWord32le 0x07
encodeChainwebVersion v = error $ "chainweb version " <> unpack v <> " does not exist"
{-# INLINE encodeChainwebVersion #-}

-- | Decode a BlockHeader
--
decodeBlockHeader :: Get BlockHeader
decodeBlockHeader = do
  nonce <- getWord64le
  time <- decodePosixTime
  parent <- decodeHash
  adjacents <- decodeAdjacents
  target <- decodeHashNat
  payloadHash <- decodeHash
  chain <- decodeChainId
  weight <- decodeHashNat
  height <- fromIntegral <$> getWord32le
  version <- decodeChainwebVersion
  epoch <- decodePosixTime
  flags <- getWord64le
  blockHash <- decodeHash

  return BlockHeader
    { _blockHeader_creationTime = time
    , _blockHeader_parent = parent
    , _blockHeader_height = height
    , _blockHeader_hash = blockHash
    , _blockHeader_chainId = chain
    , _blockHeader_weight = weight
    , _blockHeader_epochStart = epoch
    , _blockHeader_neighbors = adjacents
    , _blockHeader_payloadHash = payloadHash
    , _blockHeader_chainwebVer = version
    , _blockHeader_target = target
    , _blockHeader_flags = flags
    , _blockHeader_nonce = nonce
    }

decodePosixTime :: Get POSIXTime
decodePosixTime
  = realToFrac . (/ 1000000) . fromIntegral @_ @Double <$> getWord64le
{-# INLINE decodePosixTime #-}

decodeHash :: Get Hash
decodeHash = Hash <$> getBytes 32
{-# INLINE decodeHash #-}

decodeChainId :: Get ChainId
decodeChainId = ChainId . fromIntegral <$> getWord32le
{-# INLINE decodeChainId #-}

decodeHashNat :: Get BytesLE
decodeHashNat = BytesLE <$> getBytes 32
{-# INLINE decodeHashNat #-}

decodeAdjacents :: Get (M.Map ChainId Hash)
decodeAdjacents = do
  len <- fromIntegral <$> getWord16le
  fmap M.fromList $ replicateM len $ (,)
    <$> decodeChainId
    <*> decodeHash
{-# INLINE decodeAdjacents #-}

decodeChainwebVersion :: Get Text
decodeChainwebVersion = getWord32le >>= \case
  0x01 -> return "development"
  0x05 -> return "mainnet01"
  0x07 -> return "testnet04"
  x -> fail $ "chainweb version " <> show x <> " does not exist"
{-# INLINE decodeChainwebVersion #-}

