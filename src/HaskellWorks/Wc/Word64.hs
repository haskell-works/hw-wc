{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Wc.Word64
  ( w8sNonZero
  , w8sZero
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1

w8sNonZero :: Word64 -> Word64
w8sNonZero w = popCount1 d
  where a = w .|. (w .>. 4)
        b = a .|. (a .>. 2)
        c = b .|. (b .>. 1)
        d = c .&. 0x0101010101010101

w8sZero :: Word64 -> Word64
w8sZero w = popCount1 d
  where a = w .|. (w .>. 4)
        b = a .|. (a .>. 2)
        c = b .|. (b .>. 1)
        d = comp (c .&. 0x0101010101010101) .&. 0x0101010101010101
