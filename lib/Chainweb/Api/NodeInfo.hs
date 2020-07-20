{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Chainweb.Api.NodeInfo where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
------------------------------------------------------------------------------
import           Chainweb.Api.ChainId
import           Chainweb.Api.Common
------------------------------------------------------------------------------


data NodeInfo = NodeInfo
  { _nodeInfo_chainwebVer :: Text
  , _nodeInfo_apiVer      :: Text
  , _nodeInfo_chains      :: Set ChainId
  , _nodeInfo_numChains   :: Int
  , _nodeInfo_graphs      :: Maybe [(BlockHeight, [(Int, [Int])])]
  } deriving (Eq,Ord,Show)

atBlockHeight :: BlockHeight -> [(BlockHeight, a)] -> a
atBlockHeight _ [] = error "Empty block heightlist (should never happen)"
atBlockHeight _ [(_,g)] = g
atBlockHeight bh ((h,g):gs) = if bh >= h then g else atBlockHeight bh gs

nodeInfoCurChains :: BlockHeight -> NodeInfo -> Set ChainId
nodeInfoCurChains bh si = maybe (_nodeInfo_chains si) (S.fromList . map (ChainId . fst) . snd . head . dropWhile (\(h,_) -> bh < h)) $ _nodeInfo_graphs si

nodeInfoCurNumChains :: BlockHeight -> NodeInfo -> Int
nodeInfoCurNumChains bh si = maybe (_nodeInfo_numChains si) (length . snd . head . dropWhile (\(h,_) -> h > bh)) $ _nodeInfo_graphs si

instance FromJSON NodeInfo where
  parseJSON = withObject "NodeInfo" $ \o -> NodeInfo
    <$> o .: "nodeVersion"
    <*> o .: "nodeApiVersion"
    <*> o .: "nodeChains"
    <*> o .: "nodeNumberOfChains"
    <*> o .:? "nodeGraphHistory"

