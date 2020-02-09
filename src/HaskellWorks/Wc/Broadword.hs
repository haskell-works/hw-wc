{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Wc.Broadword
  ( countsW8
  , countsW64
  , countLinesDvsW8
  , countLinesDvsW64
  , countLinesBs
  , countLinesLbss
  , w64CountChars
  , parCountLinesLbss
  ) where

import Data.Foldable
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1

import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.Vector.AsVector64 as DVS
import qualified HaskellWorks.Data.Vector.AsVector8  as DVS
import qualified HaskellWorks.Wc.Char                as C
import qualified HaskellWorks.Wc.Counts              as Z
import qualified HaskellWorks.Wc.List                as L
import qualified HaskellWorks.Wc.Word64              as W64

countsW8 :: Word8 -> Z.Counts
countsW8 w = Z.Counts
  { Z.lines = if
      | w == C.newline  -> 1
      | otherwise       -> 0
  , Z.bytes = 1
  , Z.chars = if
      | w .&. 0xc0 == 0x80  -> 0
      | otherwise           -> 1
  }

countsW64 :: Word64 -> Z.Counts
countsW64 w = Z.Counts
  { Z.lines = W64.w8sZero (w .^. 0x0a0a0a0a0a0a0a0a)
  , Z.bytes = 8
  , Z.chars = w64CountChars w
  }

w64CountChars :: Word64 -> Word64
w64CountChars w = popCount1 w2
  where w1 = (w .^. 0x8080808080808080)  .&. 0xc0c0c0c0c0c0c0c0
        w2 = ((w1 .>. 7) .|. (w1 .>. 6)) .&. 0x0101010101010101

countLinesDvsW8 :: DVS.Vector Word8 -> Z.Counts
countLinesDvsW8 v = DVS.foldl' go mempty v
  where go :: Z.Counts -> Word8 -> Z.Counts
        go a w = a <> countsW8 w

countLinesDvsW64 :: DVS.Vector Word64 -> Z.Counts
countLinesDvsW64 v = DVS.foldl' go mempty v
  where go :: Z.Counts -> Word64 -> Z.Counts
        go a w = a <> countsW64 w

countLinesBs :: BS.ByteString -> Z.Counts
countLinesBs bs =
      countLinesDvsW64 (DVS.take  q       (DVS.asVector64 bs))
  <>  countLinesDvsW8  (DVS.drop (q * 8)  (DVS.asVector8 bs))
  where q = BS.length bs `div` 8

parCountLinesLbss :: Int -> LBS.ByteString -> Z.Counts
parCountLinesLbss n = fold . L.parLookAhead n . fmap countLinesBs . LBS.toChunks

countLinesLbss :: LBS.ByteString -> Z.Counts
countLinesLbss = fold . fmap countLinesBs . LBS.toChunks
