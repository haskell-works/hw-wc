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

newtype BroadwordOptions = BroadwordOptions
  { filePath  :: FilePath
  } deriving (Eq, Show, Generic)

newtype StateMachineOptions = StateMachineOptions
  { filePath  :: FilePath
  } deriving (Eq, Show, Generic)
