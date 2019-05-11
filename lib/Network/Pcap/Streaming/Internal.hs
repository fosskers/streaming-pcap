module Network.Pcap.Streaming.Internal where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as BS
import           Data.Word (Word32)
import           Network.Pcap

---

-- | A <https://en.wikipedia.org/wiki/Pcap pcap> packet. Assumes nothing about
-- the contents or structure of the `bytes` value.
data Packet = Packet { header :: PktHdr, bytes :: BS.ByteString } deriving (Eq, Show)

packetP :: A.Parser Packet
packetP = do
  hdr <- pktHeader
  bts <- A.take . fromIntegral $ hdrCaptureLength hdr
  pure $ Packet hdr bts

pktHeader :: A.Parser PktHdr
pktHeader = PktHdr <$> four <*> four <*> four <*> four

four :: A.Parser Word32
four = BS.foldr' (\w acc -> acc * 256 + fromIntegral w) 0 <$> A.take 4
{-# INLINE four #-}
