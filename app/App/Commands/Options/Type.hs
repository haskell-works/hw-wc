{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics
import GHC.Word     (Word8)

data SimpleOptions = SimpleOptions
  { filePath  :: FilePath
  , delimiter :: Word8
  } deriving (Eq, Show, Generic)
