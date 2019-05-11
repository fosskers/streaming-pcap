module Main ( main ) where

import           Control.Monad.Trans.Resource
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import           Network.Pcap.Streaming
import           Network.Pcap.Streaming.Internal
import qualified Streaming.Prelude as P
import           Test.Tasty
import           Test.Tasty.HUnit

---

main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Unit Tests"
  [ testGroup "Byte Parsing"
    [ testCase "pktHeader" $ A.parseOnly pktHeader heady @?= Right good
    , testCase "four" $ A.parseOnly four foury @?= Right 1338882754
    ]
  , testGroup "Streaming"
    [ testCase "Packet Quantity (test)"  $ runResourceT (P.length_ $ offline "test/test.pcap") >>= (@?= 4)
    -- , testCase "Packet Quantity (tsuru)" $ runResourceT (P.length_ $ offline "test/tsuru.pcap") >>= (@?= 21273)
    ]
  ]

foury :: BS.ByteString
foury = BS.pack [ 0xc2, 0xba, 0xcd, 0x4f ]

heady :: BS.ByteString
heady = BS.pack
  [ 0xc2, 0xba, 0xcd, 0x4f, 0xb6, 0x35, 0x0f, 0x00
  , 0x36, 0x00, 0x00, 0x00, 0x36, 0x00, 0x00, 0x00 ]

good :: PktHdr
good = PktHdr 1338882754 996790 54 54
