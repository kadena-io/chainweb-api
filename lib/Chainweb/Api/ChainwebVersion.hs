module Chainweb.Api.ChainwebVersion where


newtype ChainGraph = ChainGraph { _chainGraph :: G.DiGraph ChainId }

data ChainwebVersion
  = Development
  | Testnet04
  | Mainnet01 ChainGraph
  deriving (Eq, Ord, Show)

chainwebVersionToText :: HasCallStack => ChainwebVersion -> T.Text
chainwebVersionToText Development = "development"
chainwebVersionToText Testnet04 = "testnet04"
chainwebVersionToText Mainnet01 = "mainnet01"
{-# INLINABLE chainwebVersionToText #-}

chainwebVersionFromText :: HasCallStack => Text -> ChainwebVersion
chainwebVersionFromText "development" = Development
chainwebVersionFromText "testnet04"
