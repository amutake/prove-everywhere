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
    , coqtopState :: CoqtopState
    }

data CoqtopInfo = CoqtopInfo
    { infoCoqtopId :: Int
    , infoCoqtopOutput :: Text
    , infoCoqtopState :: CoqtopState
    } deriving (Eq, Show)

instance ToJSON CoqtopInfo where
    toJSON info = object
        [ "id" .= infoCoqtopId info
        , "output" .= infoCoqtopOutput info
        , "state" .= toJSON (infoCoqtopState info)
        ]

data ServerError
    = NoSuchCoqtopError Int
    | PromptParseError ParseError
    | RequestParseError ByteString
    | CommandError Text
    | NoSuchApiError [Text]
    deriving (Show)

instance ToJSON ServerError where
    toJSON (NoSuchCoqtopError i) = object
        [ "error" .= object
            [ "id" .= (0 :: Int)
            , "type" .= ("NoSuchCoqtopError" :: Text)
            , "message" .= ("No such coqtop id: " <> T.pack (show i))
            ]
        ]
    toJSON (PromptParseError e) = object
        [ "error" .= object
            [ "id" .= (1 :: Int)
            , "type" .= ("PromptParseError" :: Text)
            , "message" .= T.pack (show e)
            ]
        ]
    toJSON (RequestParseError t) = object
        [ "error" .= object
            [ "id" .= (2 :: Int)
            , "type" .= ("RequestParseError" :: Text)
            , "message" .= (E.decodeUtf8 t)
            ]
        ]
    toJSON (CommandError e) = object
        [ "error" .= object
            [ "id" .= (3 :: Int)
            , "type" .= ("CommandError" :: Text)
            , "message" .= e
            ]
        ]
    toJSON (NoSuchApiError ps) = object
        [ "error" .= object
            [ "id" .= (4 :: Int)
            , "type" .= ("NoSuchApiError" :: Text)
            , "message" .= T.intercalate "/" ps
            ]
        ]

data CoqtopState = CoqtopState
    { promptCurrentTheorem :: Text
    , promptWholeStateNumber :: Integer
    , promptTheoremStack :: [Text]
    , promptTheoremStateNumber :: Integer
    } deriving (Eq, Show)

instance ToJSON CoqtopState where
    toJSON prompt = object
        [ "current_theorem" .= promptCurrentTheorem prompt
        , "whole_state_number" .= promptWholeStateNumber prompt
        , "theorem_stack" .= promptTheoremStack prompt
        , "theorem_state_number" .= promptTheoremStateNumber prompt
        ]

data Command = Command Text deriving (Eq, Show)

instance FromJSON Command where
    parseJSON (Object v) = Command <$>
        v .: "command"
    parseJSON _ = mempty
