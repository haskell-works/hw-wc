{-# LANGUAGE MultiWayIf #-}

module HaskellWorks.Wc.ByteWise
  ( countChars
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable as DVS

countChars :: DVS.Vector Word8 -> Word64
countChars v = DVS.foldl go 0 v
  where go :: Word64 -> Word8 -> Word64
        go a w = a + if
          | w .&. 0xc0 == 0x80  -> 0
          | otherwise           -> 1
