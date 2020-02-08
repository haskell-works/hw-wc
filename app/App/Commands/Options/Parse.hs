{-# LANGUAGE OverloadedStrings #-}

module App.Commands.Options.Parse
  ( nonZeroOneBased
  ) where

import Data.Semigroup      ((<>))
import Data.Text
import Options.Applicative
import Text.Read           (readEither)

import qualified App.Data.List as L
import qualified Data.Text     as T

nonZeroOneBased :: Mod OptionFields Int -> Parser Int
nonZeroOneBased = option $ eitherReader $ \s -> do
  a <- readEither s
  if a == 0
    then Left "cannot index column 0"
    else Right (a - 1)
