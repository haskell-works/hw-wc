{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Broadword
  ( cmdBroadword
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Type as Z
import qualified App.IO                    as IO
import qualified HaskellWorks.Wc.Broadword as Z
import qualified HaskellWorks.Wc.Counts    as Z
import qualified System.IO                 as IO

runBroadword :: Z.BroadwordOptions -> IO ()
runBroadword opts = do
  let !filePath   = opts ^. the @"filePath"

  !lbs <- IO.readInputFile filePath

  counts <- pure $ case opts ^. the @"lookahead" of
    0 -> Z.countLinesLbss      lbs
    n -> Z.parCountLinesLbss n lbs

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
  <*> option auto
        (   long "sparks"
        <>  short 'a'
        <>  help "Number of chunks of look ahead"
        <>  metavar "STRING"
        )

cmdBroadword :: Mod CommandFields (IO ())
cmdBroadword = command "broadword"  $ flip info idm $ runBroadword <$> optsBroadword
