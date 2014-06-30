{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Types where

import Data.Aeson
import Data.Text
import Network.Wai.Handler.Warp (Port)
import System.Process (ProcessHandle)
import System.IO (Handle)

data Config = Config
    { configPort :: Port
    } deriving (Eq, Show)

data Coqtop = Coqtop
    { coqtopStdin :: Handle
    , coqtopStdout :: Handle
    , coqtopStderr :: Handle
    , coqtopProcessHandle :: ProcessHandle
    , coqtopCount :: Int
    }

data CoqtopInfo = CoqtopInfo
    { infoCoqtopNumber :: Int
    , infoCoqtopOutput :: Text
    }

instance ToJSON CoqtopInfo where
    toJSON info = object
        [ "number" .= infoCoqtopNumber info
        , "message" .= infoCoqtopOutput info
        ]
