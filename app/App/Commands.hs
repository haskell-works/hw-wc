module App.Commands where

import App.Commands.Simple
import Data.Semigroup      ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral <|> commandsDebugging

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdSimple

commandsDebugging :: Parser (IO ())
commandsDebugging = subparser $ mempty
  <>  commandGroup "Debugging commands:"
  <>  hidden
