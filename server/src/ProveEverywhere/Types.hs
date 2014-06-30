{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Types where

import Data.Aeson
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (Port)
import System.Process (ProcessHandle)
import System.IO (Handle)
import Text.Parsec (ParseError)

data Config = Config
    { configPort :: Port
    } deriving (Eq, Show)

data Coqtop = Coqtop
    { coqtopStdin :: Handle
    , coqtopStdout :: Handle
    , coqtopStderr :: Handle
    , coqtopProcessHandle :: ProcessHandle
    , coqtopCount :: Integer
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

data ServerError
    = NoSuchCoqtopError
        { errorCoqtopId :: Int
        }
    | PromptParseError
        { errorParseError :: ParseError
        }

instance ToJSON ServerError where
    toJSON (NoSuchCoqtopError i) = object
        [ "error" .= object
              [ "id" .= (0 :: Int)
              , "message" .= ("No such coqtop id: " <> T.pack (show i))
              ]
        ]
    toJSON (PromptParseError e) = object
        [ "error" .= object
              [ "id" .= (1 :: Int)
              , "message" .= T.pack (show e)
              ]
        ]

data Prompt = Prompt
    { promptCurrentTheorem :: Text
    , promptStateNumber :: Integer
    , promptTheoremStack :: [Text]
    , promptTheoremStateNumber :: Integer
    }
