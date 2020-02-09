{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Broadword
  ( cmdBroadword
  ) where

import Control.Lens
import Control.Monad
import Control.Parallel.Strategies
import Data.Generics.Product.Any
import Data.Semigroup              ((<>))
import Data.Word
import Foreign.Storable            (Storable)
import Options.Applicative         hiding (columns)

import qualified App.Commands.Options.Type         as Z
import qualified App.IO                            as IO
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.ByteString      as BS
import qualified HaskellWorks.Data.Vector.Storable as DVS
import qualified HaskellWorks.Wc.Broadword         as Z
import qualified HaskellWorks.Wc.Counts            as Z
import qualified System.IO                         as IO

splitVector :: Storable a => Int -> DVS.Vector a -> [DVS.Vector a]
splitVector n v = fmap go [0 .. n]
  where len     = DVS.length v
        partLen = (((len `div` n) + 7) `div` 8) * 8
        go i    = DVS.take partLen (DVS.drop (i * partLen) v)

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f xs = map f xs `using` parList rdeepseq

runBroadword :: Z.BroadwordOptions -> IO ()
runBroadword opts = do
  let !filePath   = opts ^. the @"filePath"

  !v8 <- DVS.mmap @Word8 filePath

  let counts :: Z.Counts = mconcat $ pmap (Z.countLinesBs . BS.toByteString) (splitVector 8 v8)

  IO.putStrLn $ "Bytes: " <> show (Z.bytes counts)
  IO.putStrLn $ "Lines: " <> show (Z.lines counts)
  IO.putStrLn $ "Chars: " <> show (Z.chars counts)

  return ()

optsBroadword :: Parser Z.BroadwordOptions
optsBroadword = Z.BroadwordOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input file"
        <>  metavar "STRING"
        )

cmdBroadword :: Mod CommandFields (IO ())
cmdBroadword = command "broadword"  $ flip info idm $ runBroadword <$> optsBroadword
