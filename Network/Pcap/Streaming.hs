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
  , PktHdr(..)
  , offline
  , online
  ) where

import qualified Data.Attoparsec.ByteString.Streaming as A
import qualified Data.ByteString.Streaming as Q
import           Data.Int (Int64)
import           Network.Pcap
import           Network.Pcap.Streaming.Internal
import           Streaming
import qualified Streaming.Prelude as P

---

-- | Read `Packet`s from some file dump.
offline :: MonadResource m => FilePath -> Stream (Of Packet) m ()
offline = void . A.parsed packetP . Q.drop 24 . Q.readFile

-- | Read `Packet`s from some network device. See `Network.Pcap.openLive`
-- for a description of each argument.
online :: MonadIO m => String -> Int -> Bool -> Int64 -> Stream (Of Packet) m ()
online n s p t = liftIO (openLive n s p t) >>= packets

packets :: MonadIO m => PcapHandle -> Stream (Of Packet) m ()
packets h = do
  (hdr,bs) <- liftIO (nextBS h)
  if hdrCaptureLength hdr == 0 then pure () else P.yield (Packet hdr bs) *> packets h
