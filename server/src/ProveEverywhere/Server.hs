{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Server where

import Control.Concurrent.MVar
import Data.Aeson
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
server coqtopMap seed req respond =
    case pathInfo req of
        ["start"] -> start
        _ -> respond $ responseLBS status404 [] ""
  where
    start = do
        n <- fresh seed
        (coqtop, o) <- startCoqtop
        insert coqtopMap n coqtop
        let res = encode CoqtopInfo
                { infoCoqtopNumber = n
                , infoCoqtopOutput = o
                }
        respond $ responseLBS status200 [] res

fresh :: MVar Int -> IO Int
fresh seed = modifyMVar seed (\n -> return (n + 1, n))

insert :: MVar CoqtopMap -> Int -> Coqtop -> IO ()
insert coqtopMap n coqtop = modifyMVar_ coqtopMap (return . HM.insert n coqtop)
