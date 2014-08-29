{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Server where

import Prelude hiding (lookup)
import Control.Concurrent.MVar
import Control.Exception
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)

import ProveEverywhere.Util
import ProveEverywhere.Types
import ProveEverywhere.Coqtop

type CoqtopMap = HashMap Int Coqtop

runServer :: Config -> IO ()
runServer config = do
    coqtopMap <- newMVar HM.empty
    seed <- newMVar 0
    run (configPort config) (server config coqtopMap seed)

server :: Config -> MVar CoqtopMap -> MVar Int -> Application
server config coqtopMap seed req respond = handle unknownError $
    case pathInfo req of
        ["start"] -> start
        ["command", n] | isNatural n -> command $ read $ T.unpack n
        ["terminate", n] | isNatural n -> terminate $ read $ T.unpack n
        paths -> do
            let res = NoSuchApiError paths
            respond $ errorResponse res
  where
    initialResponse (coqtop, o) = do
        let n = coqtopId coqtop
        insert coqtopMap n coqtop
        return $ responseJSON status200 InitialInfo
            { initialInfoId = n
            , initialInfoOutput = o
            , initialInfoState = coqtopState coqtop
            }

    commandResponse (coqtop, output) = do
        insert coqtopMap (coqtopId coqtop) coqtop
        return $ responseJSON status200 output

    unknownError :: SomeException -> IO ResponseReceived
    unknownError = respond . errorResponse . UnknownError . T.pack . show

    start = case configMaxNumProcs config of
        Nothing -> start'
        Just limit -> do
            n <- size coqtopMap
            if n < limit then start' else
                respond $ errorResponse $ ExceededMaxProcsError limit
      where
        start' = do
            n <- fresh seed
            res <- handleError (startCoqtop n) initialResponse
            respond res

    command n = do
        res <- withCoqtop coqtopMap n $ \coqtop -> do
            withDecodedBody req $ \cmd -> do
                handleError (commandCoqtop coqtop cmd) commandResponse
        respond res

    terminate n = do
        res <- withCoqtop coqtopMap n $ \coqtop -> do
            terminateCoqtop coqtop
            delete coqtopMap n
            return $ responseJSON status200 EmptyObject
        respond res

fresh :: MVar Int -> IO Int
fresh seed = modifyMVar seed (\n -> return (n + 1, n))

insert :: MVar CoqtopMap -> Int -> Coqtop -> IO ()
insert coqtopMap n coqtop = modifyMVar_ coqtopMap (return . HM.insert n coqtop)

lookup :: MVar CoqtopMap -> Int -> IO (Maybe Coqtop)
lookup coqtopMap n = withMVar coqtopMap $ return . HM.lookup n

delete :: MVar CoqtopMap -> Int -> IO ()
delete coqtopMap n = modifyMVar_ coqtopMap (return . HM.delete n)

size :: MVar CoqtopMap -> IO Int
size coqtopMap = withMVar coqtopMap $ return . HM.size

responseJSON :: ToJSON a => Status -> a -> Response
responseJSON status a =
    responseLBS status [(hContentType, "application/json")] (encode a)

withCoqtop :: MVar CoqtopMap -> Int -> (Coqtop -> IO Response) -> IO Response
withCoqtop coqtopMap n cont = do
    result <- lookup coqtopMap n
    maybe
        (return $ errorResponse $ NoSuchCoqtopError n)
        cont
        result

handleError :: IO (Either ServerError a) -> (a -> IO Response) -> IO Response
handleError io cont = io >>= either (return . errorResponse) cont

errorResponse :: ServerError -> Response
errorResponse e@(NoSuchCoqtopError _) = responseJSON status404 e
errorResponse e@(PromptParseError _) = responseJSON status500 e
errorResponse e@(RequestParseError _) = responseJSON status400 e
errorResponse e@(NoSuchApiError _) = responseJSON status404 e
errorResponse e@(UnknownError _) = responseJSON status500 e
errorResponse e@(ExceededMaxProcsError _) = responseJSON status400 e

withDecodedBody :: FromJSON a => Request -> (a -> IO Response) -> IO Response
withDecodedBody req cont = do
    body <- requestBody req
    maybe
        (return $ errorResponse $ RequestParseError body)
        cont
        (decodeStrict body)
