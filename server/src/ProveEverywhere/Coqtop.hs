{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Coqtop where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Monoid
import System.Process
import System.IO

import ProveEverywhere.Util
import ProveEverywhere.Types
import ProveEverywhere.Parser

startCoqtop :: Int -> IO (Either ServerError (Coqtop, Text))
startCoqtop n = do
    let cmd = (shell "coqtop -emacs")
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    (Just inp, Just out, Just err, ph) <- createProcess cmd
    result <- hGetOutputPair (out, err)
    return $ flip fmap result $ \(o, p) -> do
        let coqtop = Coqtop
                { coqtopId = n
                , coqtopStdin = inp
                , coqtopStdout = out
                , coqtopStderr = err
                , coqtopProcessHandle = ph
                , coqtopState = p
                }
        (coqtop, o)

commandCoqtop :: Coqtop -> Command -> IO (Either ServerError (Coqtop, CoqtopOutput))
commandCoqtop coqtop (Command cmd) = loop coqtop 0 Nothing $ splitCommands cmd
  where
    loop :: Coqtop -- ^ coqtop with last state
         -> Int -- ^ accumulator of the number of succeeded commands
         -> Maybe Output -- ^ last output
         -> [Text] -- ^ remaining commands
         -> IO (Either ServerError (Coqtop, CoqtopOutput))
    loop coqtop' acc lastOut [] = do
        let coqOut = CoqtopOutput
                { coqtopOutputId = coqtopId coqtop'
                , coqtopOutputSucceeded = acc
                , coqtopOutputRemaining = 0
                , coqtopOutputLast = lastOut
                , coqtopOutputError = Nothing
                , coqtopOutputState = coqtopState coqtop'
                }
        return $ Right (coqtop', coqOut)
    loop coqtop' acc lastOut (t:ts) = do
        result <- putAndGet coqtop' t
        case result of
            Left err -> return $ Left err
            Right (state, output) -> do
                if outputType output == ErrorOutput
                    then do
                        let coqOut = CoqtopOutput
                                { coqtopOutputId = coqtopId coqtop'
                                , coqtopOutputSucceeded = acc
                                , coqtopOutputRemaining = length (t:ts)
                                , coqtopOutputLast = lastOut
                                , coqtopOutputError = Just output
                                , coqtopOutputState = coqtopState coqtop'
                                }
                        return $ Right (coqtop', coqOut)
                    else loop (coqtop' { coqtopState = state }) (acc + 1) (Just output) ts

putAndGet :: Coqtop -> Text -> IO (Either ServerError (CoqtopState, Output))
putAndGet coqtop t = do
    B.hPutStrLn inp (E.encodeUtf8 t)
    hFlush inp
    result <- hGetOutputPair (out, err)
    return $ flip fmap result $ \(o, after) ->
        (after, mkOutput before after o)
  where
    inp = coqtopStdin coqtop
    out = coqtopStdout coqtop
    err = coqtopStderr coqtop
    before = coqtopState coqtop

mkOutput :: CoqtopState -- ^ before
         -> CoqtopState -- ^ after
         -> Text -- ^ raw output
         -> Output -- ^ output with output type
mkOutput before after output
    | before == after = Output
        { outputType = ErrorOutput
        , outputText = output
        }
    | promptTheoremStateNumber before /=
      promptTheoremStateNumber after = Output
        { outputType = ProofOutput
        , outputText = output
        }
    | otherwise = Output
        { outputType = InfoOutput
        , outputText = output
        }

terminateCoqtop :: Coqtop -> IO ()
terminateCoqtop coqtop = do
    hClose $ coqtopStdin coqtop
    hClose $ coqtopStdout coqtop
    hClose $ coqtopStderr coqtop
    terminateProcess $ coqtopProcessHandle coqtop

hGetOutput :: Handle -> IO ByteString
hGetOutput handle = hReady handle >>= handler
  where
    handler True = do
        h <- B.hGet handle 1
        t <- hGetOutput handle
        return (h <> t)
    handler False = return mempty

hGetOutputPair :: (Handle, Handle) -> IO (Either ServerError (Text, CoqtopState))
hGetOutputPair (out, err) = do
    hWait err
    p <- T.strip . E.decodeUtf8 <$> hGetOutput err
    o <- T.strip . E.decodeUtf8 <$> hGetOutput out
    case parsePrompt p of
        Left perr -> return $ Left $ PromptParseError perr
        Right prompt -> return $ Right (o, prompt)

hWait :: Handle -> IO ()
hWait handle = hWaitForInput handle 100 >>= handler
  where
    handler True = return ()
    handler False = hWait handle
