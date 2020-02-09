{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type
  ( SimpleOptions(..)
  , BroadwordOptions(..)
  , StateMachineOptions(..)
  ) where

import GHC.Generics

newtype SimpleOptions = SimpleOptions
  { filePath  :: FilePath
  } deriving (Eq, Show, Generic)

data BroadwordOptions = BroadwordOptions
  { filePath  :: FilePath
  , lookahead :: Int
  } deriving (Eq, Show, Generic)

newtype StateMachineOptions = StateMachineOptions
  { filePath  :: FilePath
  } deriving (Eq, Show, Generic)
