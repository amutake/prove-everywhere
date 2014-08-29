
module Main where

import Control.Exception
import Options.Applicative
import System.Process

import ProveEverywhere.Types
import ProveEverywhere.Server

main :: IO ()
main = do
    config <- execParser opts
    result <- getCoqtopVersion
    case result of
        Nothing -> putStrLn "coqtop not found"
        Just (n, m, p) | (n, m, p) < (8, 4, 0) -> do
            putStrLn $ concat
                [ "coqtop (" ++ show n ++ "." ++ show m ++ "pl" ++ show p ++ ") found\n"
                , "but this server does not support this version"
                ]
        Just (n, m, p) -> do
            putStrLn $ "coqtop (" ++ show n ++ "." ++ show m ++ "pl" ++ show p ++ ") found"
            putStrLn $ "Server started on port " ++ show (configPort config)
            runServer config
  where
    opts = info (helper <*> configParser) $
        fullDesc <> header "prove-everywhere-server - The server for ProveEverywhere"

configParser :: Parser Config
configParser = Config
    <$> option (long "port" <>
                short 'p' <>
                metavar "PORT" <>
                help "Specify port number")
    <*> optional (option
                  (long "number" <>
                   short 'n' <>
                   metavar "NUM_OF_PROCS" <>
                   help "Max number of coqtop processes (default: infinity)"))
    <*> optional (option
                  (long "time" <>
                   short 't' <>
                   metavar "KILL_TIME" <>
                   help "The time to terminate unused coqtop process (unit of time: minute) (default: infinity)"))

getCoqtopVersion :: IO (Maybe (Int, Int, Int)) -- e.g., (8, 4, 4) = 8.4pl4
getCoqtopVersion = handle failure $ do
    v_all <- readProcess "coqtop" ["-v"] ""
    let [n_str, '.', m_str, 'p', 'l', p_str]
            = drop (length "The Coq Proof Assistant, version ")
            . take (length "The Coq Proof Assistant, version *.*pl*")
            $ v_all
    let (n, m, p) = (read [n_str], read [m_str], read [p_str])
    return (Just (n, m, p))
  where
    failure :: IOError -> IO (Maybe (Int, Int, Int))
    failure _ = return Nothing
