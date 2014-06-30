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
        _ -> respond $ responseLBS status404 [] ""
  where
    start = do
        n <- fresh seed
        res <- handleError startCoqtop $ \(coqtop, o) -> do
            insert coqtopMap n coqtop
            return $ responseJSON status200 CoqtopInfo
                { infoCoqtopId = n
                , infoCoqtopOutput = o
                }
        respond res

    command n = do
        lookup coqtopMap n >>= \case
            Nothing -> do
                let res = NoSuchCoqtopError
                        { errorCoqtopId = n
                        }
                respond $ responseJSON status404 res
            Just coqtop -> do
                body <- requestBody req
                case decodeStrict body of
                    Nothing -> do
                        let res = CannotParseRequestError
                                { errorRequest = body
                                }
                        respond $ responseJSON status400 res
                    Just cmd -> do
                        res <- handleError (commandCoqtop coqtop cmd) $ \(coqtop', o) -> do
                            update coqtopMap n coqtop'
                            return $ responseJSON status200 CoqtopInfo
                                { infoCoqtopId = n
                                , infoCoqtopOutput = o
                                }
                        respond res

    terminate n = do
        lookup coqtopMap n >>= \case
            Nothing -> do
                let res = NoSuchCoqtopError
                        { errorCoqtopId = n
                        }
                respond $ responseJSON status404 res
            Just coqtop -> do
                terminateCoqtop coqtop
                delete coqtopMap n
                respond $ responseLBS status200 [] "ok"

fresh :: MVar Int -> IO Int
fresh seed = modifyMVar seed (\n -> return (n + 1, n))

insert :: MVar CoqtopMap -> Int -> Coqtop -> IO ()
insert coqtopMap n coqtop = modifyMVar_ coqtopMap (return . HM.insert n coqtop)

lookup :: MVar CoqtopMap -> Int -> IO (Maybe Coqtop)
lookup coqtopMap n = withMVar coqtopMap $ return . HM.lookup n

delete :: MVar CoqtopMap -> Int -> IO ()
delete coqtopMap n = modifyMVar_ coqtopMap (return . HM.delete n)

update :: MVar CoqtopMap -> Int -> Coqtop -> IO ()
update = insert

responseJSON :: ToJSON a => Status -> a -> Response
responseJSON status a =
    responseLBS status [(hContentType, "application/json")] (encode a)

handleError :: IO (Either ServerError a) -> (a -> IO Response) -> IO Response
handleError io cont = io >>= \case
    Left e -> return $ responseJSON status500 e
    Right a -> cont a
