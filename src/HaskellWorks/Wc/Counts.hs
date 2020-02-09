{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellWorks.Wc.Counts
  ( Counts(..)
  ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import Prelude         hiding (lines)

data Counts = Counts
  { bytes :: !Word64
  , lines :: !Word64
  , chars :: !Word64
  } deriving (Eq, Show, Generic)

deriving instance NFData Counts

instance Semigroup Counts where
  a <> b = Counts
    { bytes = bytes a + bytes b
    , lines = lines a + lines b
    , chars = chars a + chars b
    }

instance Monoid Counts where
  mempty = Counts
    { bytes = 0
    , lines = 0
    , chars = 0
    }
