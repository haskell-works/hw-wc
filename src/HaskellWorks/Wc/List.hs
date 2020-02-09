{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Wc.List
  ( parLookAhead
  ) where

import Control.Parallel.Strategies

interleave :: [a] -> [a] -> [a]
interleave (a:as) (b:bs) = a:b:interleave as bs
interleave as []         = as
interleave [] bs         = bs

spark :: a -> Either () a
spark a = runEval $ do
  _ <- rpar a
  return (Left ())

parLookAhead :: Int -> [a] -> [a]
parLookAhead n as = parLookAhead' $ (fmap spark (take n as)) ++ interleave (fmap Right as) (fmap spark (drop n as))

parLookAhead' :: [Either () a] -> [a]
parLookAhead' (Left  _:as) = parLookAhead' as
parLookAhead' (Right a:as) = a:parLookAhead' as
parLookAhead' []           = []
