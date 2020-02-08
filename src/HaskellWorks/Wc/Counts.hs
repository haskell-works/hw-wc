module HaskellWorks.Wc.Counts
  ( Counts(..)
  ) where

import Data.Word
import Prelude   hiding (lines)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

data Counts = Counts
  { bytes :: !Word64
  , lines :: !Word64
  , chars :: !Word64
  } deriving (Eq, Show)

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
