{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module ProveEverywhere.Server where

import Prelude hiding (lookup)
import Control.Concurrent.MVar
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.HTTP.Types
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
        ["command", n] -> command $ read $ T.unpack n
        ["terminate", n] -> terminate $ read $ T.unpack n
        paths -> do
            let res = NoSuchApiError paths
            respond $ responseJSON status404 res
  where
    successResponse n (coqtop, o) = do
        insert coqtopMap n coqtop
        return $ responseJSON status200 CoqtopInfo
            { infoCoqtopId = n
            , infoCoqtopOutput = o
            , infoCoqtopState = coqtopState coqtop
            }

    start = do
        n <- fresh seed
        res <- handleError startCoqtop $ successResponse n
        respond res

    command n = do
        res <- withCoqtop coqtopMap n $ \coqtop -> do
            withDecodedBody req $ \cmd -> do
                handleError (commandCoqtop coqtop cmd) $ successResponse n
        respond res

    terminate n = do
        res <- withCoqtop coqtopMap n $ \coqtop -> do
            terminateCoqtop coqtop
            delete coqtopMap n
            return $ responseLBS status200 [] "ok"
        respond res

fresh :: MVar Int -> IO Int
fresh seed = modifyMVar seed (\n -> return (n + 1, n))

insert :: MVar CoqtopMap -> Int -> Coqtop -> IO ()
insert coqtopMap n coqtop = modifyMVar_ coqtopMap (return . HM.insert n coqtop)

lookup :: MVar CoqtopMap -> Int -> IO (Maybe Coqtop)
lookup coqtopMap n = withMVar coqtopMap $ return . HM.lookup n

delete :: MVar CoqtopMap -> Int -> IO ()
delete coqtopMap n = modifyMVar_ coqtopMap (return . HM.delete n)

responseJSON :: ToJSON a => Status -> a -> Response
responseJSON status a =
    responseLBS status [(hContentType, "application/json")] (encode a)

withCoqtop :: MVar CoqtopMap -> Int -> (Coqtop -> IO Response) -> IO Response
withCoqtop coqtopMap n cont = do
    result <- lookup coqtopMap n
    maybe
        (return $ responseJSON status404 $ NoSuchCoqtopError n)
        cont
        result

handleError :: IO (Either ServerError a) -> (a -> IO Response) -> IO Response
handleError io cont = io >>= either (return . responseJSON status500) cont

withDecodedBody :: FromJSON a => Request -> (a -> IO Response) -> IO Response
withDecodedBody req cont = do
    body <- requestBody req
    maybe
        (return $ responseJSON status400 $ RequestParseError body)
        cont
        (decodeStrict body)
