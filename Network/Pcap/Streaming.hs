-- |
-- Module    : Network.Pcap.Streaming
-- Copyright : (c) Colin Woodbury, 2018
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- A streaming interface to the <http://hackage.haskell.org/package/pcap-0.4.5.2 pcap>
-- Haskell library, which itself is a binding to /libpcap/. Humbly adapted from
-- <http://hackage.haskell.org/package/pcap-conduit pcap-conduit> by Austin Seipp.

module Network.Pcap.Streaming
  ( Packet(..)
  , offline
  , online
  ) where

import qualified Data.ByteString as BS
import           Data.Int (Int64)
import           Network.Pcap
import           Streaming
import qualified Streaming.Prelude as P

---

-- | A <https://en.wikipedia.org/wiki/Pcap pcap> packet. Assumes nothing about
-- the contents or structure of the `bytes` value.
data Packet = Packet { header :: PktHdr, bytes :: BS.ByteString } deriving (Eq, Show)

-- | Read `Packet`s from some file dump. See also `Network.Pcap.openOffline`.
offline :: FilePath -> Stream (Of Packet) IO ()
offline path = lift (openOffline path) >>= packets

-- | Read `Packet`s from some network device. See `Network.Pcap.openLive`
-- for a description of each argument.
online :: String -> Int -> Bool -> Int64 -> Stream (Of Packet) IO ()
online n s p t = lift (openLive n s p t) >>= packets

packets :: PcapHandle -> Stream (Of Packet) IO ()
packets h = do
  (hdr,bs) <- lift (nextBS h)
  if hdrCaptureLength hdr == 0 then pure () else P.yield (Packet hdr bs) *> packets h
