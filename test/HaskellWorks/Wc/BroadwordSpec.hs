{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Wc.BroadwordSpec
  ( spec
  ) where

import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                     as BS
import qualified HaskellWorks.Data.Vector.AsVector64 as DVS
import qualified HaskellWorks.Data.Vector.AsVector8  as DVS
import qualified HaskellWorks.Wc.Broadword           as BW
import qualified HaskellWorks.Wc.ByteWise            as BW
import qualified Hedgehog.Gen                        as G
import qualified Hedgehog.Range                      as R

{- HLINT ignore "Redundant do"  -}

spec :: Spec
spec = describe "HaskellWorks.Wc.BroadwordSpec" $ do
  it "stuff" $ requireProperty $ do
    bs          <- forAll $ BS.pack <$> G.list (R.singleton 8) (G.word8 R.constantBounded)
    BitShown w  <- forAll $ pure $ BitShown $ DVS.asVector64 bs !!! 0

    BW.w64CountChars w === BW.countChars (DVS.asVector8 bs)
