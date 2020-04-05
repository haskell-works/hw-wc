{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main

makeBenchWc :: IO [Benchmark]
makeBenchWc = return []

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [makeBenchWc]
  defaultMain benchmarks
