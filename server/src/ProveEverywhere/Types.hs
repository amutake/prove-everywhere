{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Types where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
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
    , coqtopStateNumber :: Integer
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
    | CannotParseRequestError
        { errorRequest :: ByteString
        }
    | CommandError
        { errorCommandMessage :: Text
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
    toJSON (CannotParseRequestError t) = object
        [ "error" .= object
            [ "id" .= (2 :: Int)
            , "message" .= ("Cannot parse request: " <> E.decodeUtf8 t)
            ]
        ]
    toJSON (CommandError e) = object
        [ "error" .= object
            [ "id" .= (3 :: Int)
            , "message" .= e
            ]
        ]

data Prompt = Prompt
    { promptCurrentTheorem :: Text
    , promptStateNumber :: Integer
    , promptTheoremStack :: [Text]
    , promptTheoremStateNumber :: Integer
    }

data Command = Command Text

instance FromJSON Command where
    parseJSON (Object v) = Command <$>
        v .: "command"
    parseJSON _ = mempty
