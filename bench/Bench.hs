module Main where

import           Criterion.Main
import           Streaming (runResourceT)
import qualified Streaming.Prelude as P
import           Network.Pcap.Streaming

---

main :: IO ()
main = defaultMain
  [ bgroup "Benchmarks"
    [ bench "Count all entries in stream" . nfIO $ runResourceT (P.length_ $ offline "test/test.pcap")
    , bench "Sum all timestamps" . nfIO $ runResourceT (P.sum_ . P.map (hdrSeconds . header) $ offline "test/test.pcap")
    ]
  ]
