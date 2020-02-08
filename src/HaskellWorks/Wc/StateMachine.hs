{-# LANGUAGE MultiWayIf #-}

{-# OPTIONS -fno-warn-overflowed-literals #-}

module HaskellWorks.Wc.StateMachine
  ( transition
  , transitionTable
  , phiTable
  , countLinesLbs
  ) where

import Data.Foldable
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as LBS
import qualified Data.Vector.Storable   as DVS
import qualified HaskellWorks.Wc.Counts as Z

data State = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | E deriving (Eq, Show, Enum)

transition :: State -> Word8 -> State
transition E _ = E
transition R0 w = if
  | w .&. 0xff == 0xff -> R7
  | w .&. 0xff == 0xfe -> R6
  | w .&. 0xfe == 0xfc -> R5
  | w .&. 0x80 == 0x00 -> R0
  | w .&. 0xfc == 0xf8 -> R4
  | w .&. 0xf8 == 0xf0 -> R3
  | w .&. 0xf0 == 0xe0 -> R2
  | w .&. 0xe0 == 0xc0 -> R1
  | otherwise          -> E
transition R7 w = if w .&. 0x80 == 0x00 then R6 else E
transition R6 w = if w .&. 0x80 == 0x00 then R5 else E
transition R5 w = if w .&. 0x80 == 0x00 then R4 else E
transition R4 w = if w .&. 0x80 == 0x00 then R3 else E
transition R3 w = if w .&. 0x80 == 0x00 then R2 else E
transition R2 w = if w .&. 0x80 == 0x00 then R1 else E
transition R1 w = if w .&. 0x80 == 0x00 then R0 else E

transitionTable :: DVS.Vector Word8
transitionTable = DVS.generate (256 * 8) go
  where go :: Int -> Word8
        go i = fromIntegral (fromEnum (transition (toEnum (i `div` 256)) (fromIntegral (i `mod` 256))))
{-# NOINLINE transitionTable #-}

phiNewline :: State -> Word8 -> Word8
phiNewline R0 10 = 1
phiNewline _  _  = 0

phiByte :: State -> Word8 -> Word8
phiByte _ _ = 1

phiChar :: State -> Word8 -> Word8
phiChar R0 _ = 1
phiChar _  _ = 0

phiTable :: DVS.Vector Word8
phiTable = DVS.generate (256 * 8) go
  where go :: Int -> Word8
        go i = (nl .<. 2) .|. (b .<. 1) .|. c
          where nl  = phiNewline s w
                b   = phiByte    s w
                c   = phiChar    s w
                s   = toEnum (i `div` 256)
                w   = fromIntegral (i `mod` 256)
{-# NOINLINE phiTable #-}

lookupCount :: State -> Word8 -> Z.Counts
lookupCount s w = Z.Counts
  { Z.lines = (u .>. 2) .&. 1
  , Z.bytes = (u .>. 1) .&. 1
  , Z.chars =  u        .&. 1
  }
  where u :: Word64
        u = fromIntegral (phiTable !!! i)
        i = fromIntegral (fromEnum s * 256 + fromIntegral w)

lookupState :: State -> Word8 -> State
lookupState s w = toEnum (fromIntegral (transitionTable !!! i))
  where i = fromIntegral (fromEnum s * 256 + fromIntegral w)

countLinesBs :: State -> BS.ByteString -> (Z.Counts, State)
countLinesBs s = BS.foldl' go (mempty, s)
  where go :: (Z.Counts, State) -> Word8 -> (Z.Counts, State)
        go (c, t) w = (c <> lookupCount t w, lookupState t w)

countLinesBss :: State -> [BS.ByteString] -> (Z.Counts, State)
countLinesBss s bss = foldl' go (mempty, s) bss
  where go :: (Z.Counts, State) -> BS.ByteString -> (Z.Counts, State)
        go (c, t) bs = let (c', t') = countLinesBs t bs in (c <> c', t')

countLinesLbs' :: State -> LBS.ByteString -> (Z.Counts, State)
countLinesLbs' s lbs = countLinesBss s (LBS.toChunks lbs)

countLinesLbs :: LBS.ByteString -> (Z.Counts, State)
countLinesLbs = countLinesLbs' R0
