{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Types where

import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
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
    { infoCoqtopId :: Int
    , infoCoqtopOutput :: Text
    }

instance ToJSON CoqtopInfo where
    toJSON info = object
        [ "id" .= infoCoqtopId info
        , "message" .= infoCoqtopOutput info
        ]

data ServerError = NoSuchCoqtopError
    { errorCoqtopId :: Int
    }

instance ToJSON ServerError where
    toJSON (NoSuchCoqtopError i) = object
        [ "id" .= (0 :: Int)
        , "message" .= ("No such coqtop id: " <> T.pack (show i))
        ]
