{-# language ApplicativeDo #-}

module Main where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NEL
import Options.Applicative
import System.Process.Typed

data Command
    = Exec ExecArgs

data ExecArgs = ExecArgs
    { execArgsScript :: NonEmpty String
    }

optionsParser :: ParserInfo Command
optionsParser =
    info' parser' "Welcome to `hotel-california`"
  where
  -- thanks danidiaz for the blog post
    info' :: Parser a -> String -> ParserInfo a
    info' p desc = info
        (helper <*> p)
        (fullDesc <> progDesc desc)

    parser' :: Parser Command
    parser' = (subparser . foldMap command')
        [ ("exec", "Execute the given command with tracing enabled", Exec <$> execParser)
        ]

    command' (cmdName,desc,parser) =
        command cmdName (info' parser desc)

    execParser :: Parser ExecArgs
    execParser = do
        execArgsScript1 <- argument str (metavar "SCRIPT")
        execArgsScriptRest <- many $ argument str (metavar "SCRIPT")
        pure ExecArgs
            { execArgsScript = execArgsScript1 :| execArgsScriptRest
            }

main :: IO ()
main = do
    opts <- execParser optionsParser
    case opts of
        Exec ExecArgs {..} -> do
            print ("args: ", execArgsScript)
            exitCode <- runProcess $ shell $ unwords $ NEL.toList execArgsScript
            print ("exit code: ", exitCode)





