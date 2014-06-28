module Main where

import Options.Applicative

import ProveEverywhere.Types
import ProveEverywhere.Server

main :: IO ()
main = do
    config <- execParser opts
    runServer config
  where
    opts = info (helper <*> configParser) $
        fullDesc <> header "prove-everywhere-server - The server for ProveEverywhere"

configParser :: Parser Config
configParser = Config
    <$> option (long "port" <> short 'p' <> metavar "PORT" <> help "Specify port number")
