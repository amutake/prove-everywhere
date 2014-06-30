{-# LANGUAGE LambdaCase #-}

module ProveEverywhere.Coqtop where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Monoid
import System.Process
import System.IO

import ProveEverywhere.Types
import ProveEverywhere.Parser

startCoqtop :: IO (Either ServerError (Coqtop, Text))
startCoqtop = do
    let cmd = (shell "coqtop -emacs")
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (Just inp, Just out, Just err, ph) <- createProcess cmd
    result <- hGetOutputPair (out, err)
    return $ flip fmap result $ \(o, p) -> do
        let coqtop = Coqtop
                { coqtopStdin = inp
                , coqtopStdout = out
                , coqtopStderr = err
                , coqtopProcessHandle = ph
                , coqtopStateNumber = promptStateNumber p
                }
        (coqtop, o)

commandCoqtop :: Coqtop -> Command -> IO (Either ServerError (Coqtop, Text))
commandCoqtop coqtop (Command t) = do
    let inp = coqtopStdin coqtop
    let out = coqtopStdout coqtop
    let err = coqtopStderr coqtop
    B.hPutStrLn inp (E.encodeUtf8 t)
    hFlush inp
    hGetOutputPair (out, err) >>= \case
        Left e -> return (Left e)
        Right (o, p) -> do
            let state = promptStateNumber p
            if state == coqtopStateNumber coqtop
                then return $ Left $ CommandError o
                else return $ Right (coqtop { coqtopStateNumber = state }, o)

terminateCoqtop :: Coqtop -> IO ()
terminateCoqtop coqtop = do
    hClose $ coqtopStdin coqtop
    hClose $ coqtopStdout coqtop
    hClose $ coqtopStderr coqtop
    terminateProcess $ coqtopProcessHandle coqtop

hGetOutput :: Handle -> IO ByteString
hGetOutput handle = hReady handle >>= \case
    True -> do
        h <- B.hGet handle 1
        t <- hGetOutput handle
        return (h <> t)
    False -> return mempty

hGetOutputPair :: (Handle, Handle) -> IO (Either ServerError (Text, Prompt))
hGetOutputPair (out, err) = do
    hWait err
    p <- T.strip . E.decodeUtf8 <$> hGetOutput err
    o <- T.strip . E.decodeUtf8 <$> hGetOutput out
    case parsePrompt p of
        Left perr -> return $ Left $ PromptParseError perr
        Right prompt -> return $ Right (o, prompt)

hWait :: Handle -> IO ()
hWait handle = hWaitForInput handle 100 >>= \case
    True -> return ()
    False -> hWait handle
