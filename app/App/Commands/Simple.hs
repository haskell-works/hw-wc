{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Simple
  ( cmdSimple
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import Options.Applicative            hiding (columns)

import qualified App.Commands.Options.Type          as Z
import qualified App.IO                             as IO
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Vector.Storable               as DVS
import qualified HaskellWorks.Data.Vector.AsVector8 as DVS
import qualified HaskellWorks.Wc.Char               as C
import qualified HaskellWorks.Wc.Counts             as Z
import qualified System.IO                          as IO

countLinesDvs :: DVS.Vector Word8 -> Z.Counts
countLinesDvs v = DVS.foldl go mempty v
  where go :: Z.Counts -> Word8 -> Z.Counts
        go a w = a <> Z.Counts
          { Z.lines = if
              | w == C.newline  -> 1
              | otherwise       -> 0
          , Z.bytes = 1
          , Z.chars = if
              | w .&. 0xc0 == 0x80  -> 0
              | otherwise           -> 1
          }

countLinesLbs :: BS.ByteString -> Z.Counts
countLinesLbs = countLinesDvs . DVS.asVector8

countLinesLbss :: LBS.ByteString -> Z.Counts
countLinesLbss = foldl (<>) mempty . fmap countLinesLbs . LBS.toChunks

runSimple :: Z.SimpleOptions -> IO ()
runSimple opts = do
  let !filePath   = opts ^. the @"filePath"

  !lbs <- IO.readInputFile filePath

  let counts = countLinesLbss lbs

  IO.putStrLn $ "Bytes: " <> show (Z.bytes counts)
  IO.putStrLn $ "Lines: " <> show (Z.lines counts)
  IO.putStrLn $ "Chars: " <> show (Z.chars counts)

  return ()

optsSimple :: Parser Z.SimpleOptions
optsSimple = Z.SimpleOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input file"
        <>  metavar "STRING"
        )

cmdSimple :: Mod CommandFields (IO ())
cmdSimple = command "simple"  $ flip info idm $ runSimple <$> optsSimple
