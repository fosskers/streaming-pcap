module Main ( main ) where

import           Network.Pcap.Streaming
import qualified Streaming.Prelude as P
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testCase "Packet Quantity" $ P.length_ (offline "test/test.pcap") >>= (@?= 21273)
  ]
