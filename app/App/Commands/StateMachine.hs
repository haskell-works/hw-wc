{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.StateMachine
  ( cmdStateMachine
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Type    as Z
import qualified App.IO                       as IO
import qualified HaskellWorks.Wc.Counts       as Z
import qualified HaskellWorks.Wc.StateMachine as Z
import qualified System.IO                    as IO

runStateMachine :: Z.StateMachineOptions -> IO ()
runStateMachine opts = do
  let !filePath   = opts ^. the @"filePath"

  !lbs <- IO.readInputFile filePath

  let (counts, _) = Z.countLinesLbs lbs

  IO.putStrLn $ "Bytes: " <> show (Z.bytes counts)
  IO.putStrLn $ "Lines: " <> show (Z.lines counts)
  IO.putStrLn $ "Chars: " <> show (Z.chars counts)

  return ()

optsStateMachine :: Parser Z.StateMachineOptions
optsStateMachine = Z.StateMachineOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input file"
        <>  metavar "STRING"
        )

cmdStateMachine :: Mod CommandFields (IO ())
cmdStateMachine = command "state-machine"  $ flip info idm $ runStateMachine <$> optsStateMachine
