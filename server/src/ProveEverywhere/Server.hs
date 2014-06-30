{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Server where

import Control.Concurrent.MVar
import Data.ByteString.Lazy (fromStrict)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Types.Status (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)

import ProveEverywhere.Types
import ProveEverywhere.Coqtop

type CoqtopMap = HashMap Int Coqtop

runServer :: Config -> IO ()
runServer config = do
    coqtopMap <- newMVar HM.empty
    seed <- newMVar 0
    run (configPort config) (server coqtopMap seed)

server :: MVar CoqtopMap -> MVar Int -> Application
server coqtopMap seed req respond = do
    let path = pathInfo req
    case path of
        ["start"] -> start
        _ -> respond $ responseLBS status404 [] ""
  where
    start = do
        n <- fresh seed
        (coqtop, o) <- startCoqtop
        insert coqtopMap n coqtop
        respond $ responseLBS status200 [] $ fromStrict o

fresh :: MVar Int -> IO Int
fresh seed = modifyMVar seed (\n -> return (n + 1, n))

insert :: MVar CoqtopMap -> Int -> Coqtop -> IO ()
insert coqtopMap n coqtop = modifyMVar_ coqtopMap (return . HM.insert n coqtop)
