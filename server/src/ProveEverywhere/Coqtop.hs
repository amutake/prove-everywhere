{-# LANGUAGE LambdaCase #-}

module ProveEverywhere.Coqtop where

import Data.ByteString (ByteString, hGet)
import Data.Monoid
import System.Process
import System.IO

import ProveEverywhere.Types

startCoqtop :: IO (Coqtop, ByteString)
startCoqtop = do
    let cmd = (shell "coqtop -emacs")
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (Just inp, Just out, Just err, ph) <- createProcess cmd
    (o, _) <- hGetOutputPair (out, err)
    let coqtop = Coqtop
            { coqtopStdin = inp
            , coqtopStdout = out
            , coqtopStderr = err
            , coqtopProcessHandle = ph
            , coqtopCount = 1
            }
    return (coqtop, o)

hGetOutput :: Handle -> IO ByteString
hGetOutput handle = hReady handle >>= \case
    True -> do
        h <- hGet handle 1
        t <- hGetOutput handle
        return (h <> t)
    False -> return mempty

hGetOutputPair :: (Handle, Handle) -> IO (ByteString, ByteString)
hGetOutputPair (out, err) = do
    hWait err
    p <- hGetOutput err
    o <- hGetOutput out
    return (o, p)

hWait :: Handle -> IO ()
hWait handle = hWaitForInput handle 100 >>= \case
    True -> return ()
    False -> hWait handle
