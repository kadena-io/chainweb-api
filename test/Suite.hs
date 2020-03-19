{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main ( main ) where

------------------------------------------------------------------------------
import           Data.Aeson
import qualified Data.Serialize as S
import           Data.Text (Text)
import           Data.Text.Encoding
import           NeatInterpolation
import           Test.Tasty
import           Test.Tasty.HUnit
------------------------------------------------------------------------------
import           Chainweb.Api.Base64Url
import           Chainweb.Api.BlockHeader
import           Chainweb.Api.BytesLE
------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
     $ testGroup "Chainweb API Tests" suite

suite :: [TestTree]
suite =
    [ testCase "Block target JSON encoding" $
        Right 14474011154664524427946373126085988481658748083205070504932198000989141204991
        @=? (headerTarget <$> eitherDecodeStrict (encodeUtf8 blockJson))
    , testCase "Block target binary encoding" $
        Right 14474011154664524427946373126085988481658748083205070504932198000989141204991
        @=? (fmap headerTarget . S.decode =<< decodeB64UrlNoPaddingText blockBinary)
    ]
  where
    headerTarget = leToInteger . unBytesLE . _blockHeader_target

blockJson :: Text
blockJson = [text|
{"creationTime":1572404750770821,"parent":"tn-ntdNtsYYcnBfoh819vJL9ldz-qkIXf4MDO6Kd-fw","height":25,"hash":"N50q51qAGwKqugD4rK5fyvDQI5-9X-t3lK3c9V8WJI8","chainId":0,"weight":"JwAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA","featureFlags":0,"epochStart":1572404743031832,"adjacents":{"2":"Bm80lWv8r5nwpzp86TGE7oYmhJ4wg4jr58KJV0JqhLk","5":"Vbogma-GMmPtrxF86kEzn8Zw4jkLxeXStekPprJXMLU","3":"MsqHenmTRs42OVDB28oP_5MZCm8RaqDSVDrsVsTo2X0"},"payloadHash":"G5olU5tYfqU7FYXw99vyqEnURagZB-f2pL9ux7gwYks","chainwebVersion":"mainnet01","target":"_________________________________________x8","nonce":"15652723681697760986"}
|]

blockBinary :: Text
blockBinary = "2obwvkGlOdmF4pwCGJYFALZ_p7XTbbGGHJwX6IfNfbyS_ZXc_qpCF3-DAzuinfn8AwACAAAABm80lWv8r5nwpzp86TGE7oYmhJ4wg4jr58KJV0JqhLkDAAAAMsqHenmTRs42OVDB28oP_5MZCm8RaqDSVDrsVsTo2X0FAAAAVbogma-GMmPtrxF86kEzn8Zw4jkLxeXStekPprJXMLX_________________________________________HxuaJVObWH6lOxWF8Pfb8qhJ1EWoGQfn9qS_bse4MGJLAAAAACcAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAGQAAAAAAAAAFAAAAGMwmAhiWBQAAAAAAAAAAADedKudagBsCqroA-KyuX8rw0COfvV_rd5St3PVfFiSP"

{-
Correct decoding:

Right (BlockHeader {_blockFlags = FeatureFlags 15652723681697760986, _blockCreationTime = BlockCreationTime {_bct = Time (TimeSpan (Micros 1572404750770821))}, _blockParent = "tn-ntdNtsYYcnBfoh819vJL9ldz-qkIXf4MDO6Kd-fw", _blockAdjacentHashes = BlockHashRecord {_getBlockHashRecord = fromList [(ChainId 5,"Vbogma-GMmPtrxF86kEzn8Zw4jkLxeXStekPprJXMLU"),(ChainId 2,"Bm80lWv8r5nwpzp86TGE7oYmhJ4wg4jr58KJV0JqhLk"),(ChainId 3,"MsqHenmTRs42OVDB28oP_5MZCm8RaqDSVDrsVsTo2X0")]}, _blockTarget = HashTarget (PowHashNat 14474011154664524427946373126085988481658748083205070504932198000989141204991), _blockPayloadHash = BlockPayloadHash (MerkleLogHash G5olU5tYfqU7FYXw99vyqEnURagZB-f2pL9ux7gwYks), _blockChainId = ChainId 0, _blockWeight = BlockWeight (HashDifficulty (PowHashNat 39)), _blockHeight = 25, _blockChainwebVersion = mainnet01, _blockEpochStart = EpochStartTime (Time (TimeSpan (Micros 1572404743031832))), _blockNonce = Nonce 0, _blockHash = "N50q51qAG
wKqugD4rK5fyvDQI5-9X-t3lK3c9V8WJI8"})
-}
