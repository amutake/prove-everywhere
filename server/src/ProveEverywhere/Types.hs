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
    { coqtopId :: Int
    , coqtopStdin :: Handle
    , coqtopStdout :: Handle
    , coqtopStderr :: Handle
    , coqtopProcessHandle :: ProcessHandle
    , coqtopState :: CoqtopState
    }

data InitialInfo = InitialInfo
    { initialInfoId :: Int -- ^ coqtop id
    , initialInfoOutput :: Text -- ^ coqtop initial output
    , initialInfoState :: CoqtopState -- ^ initial state
    } deriving (Eq, Show)

instance ToJSON InitialInfo where
    toJSON info = object
        [ "id" .= initialInfoId info
        , "output" .= initialInfoOutput info
        , "state" .= toJSON (initialInfoState info)
        ]

-- | The data type of output of coqtop.
-- done + remain == length (sent_commands)
data CoqtopOutput = CoqtopOutput
    { coqtopOutputId :: Int -- ^ coqtop id
    , coqtopOutputSucceeded :: Int -- ^ the number of succeeded commands
    , coqtopOutputRemaining :: Int -- ^ the number of remaining commands
    , coqtopOutputLast :: Maybe Output -- ^ last output (except error)
    , coqtopOutputError :: Maybe Output -- ^ error output
    , coqtopOutputState :: CoqtopState -- ^ last state
    }

instance ToJSON CoqtopOutput where
    toJSON output = object
        [ "id" .= coqtopOutputId output
        , "succeeded" .= coqtopOutputSucceeded output
        , "remaining" .= coqtopOutputRemaining output
        , "last_output" .= coqtopOutputLast output
        , "error_output" .= coqtopOutputError output
        , "state" .= coqtopOutputState output
        ]

data ServerError
    = NoSuchCoqtopError Int
    | PromptParseError ParseError
    | RequestParseError ByteString
    | NoSuchApiError [Text]
    | UnknownError Text
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
    toJSON (NoSuchApiError ps) = object
        [ "error" .= object
            [ "id" .= (3 :: Int)
            , "type" .= ("NoSuchApiError" :: Text)
            , "message" .= T.intercalate "/" ps
            ]
        ]
    toJSON (UnknownError t) = object
        [ "error" .= object
            [ "id" .= (4 :: Int)
            , "type" .= ("UnknownError" :: Text)
            , "message" .= t
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

data Output = Output
    { outputType :: OutputType
    , outputText :: Text
    } deriving (Eq, Show)

instance ToJSON Output where
    toJSON output = object
        [ "type" .= outputType output
        , "output" .= outputText output
        ]

data OutputType = ErrorOutput
                | InfoOutput
                | ProofOutput
                deriving (Eq, Show)

instance ToJSON OutputType where
    toJSON ErrorOutput = String "error"
    toJSON InfoOutput = String "info"
    toJSON ProofOutput = String "proof"

data EmptyObject = EmptyObject deriving (Eq, Show)

instance ToJSON EmptyObject where
    toJSON EmptyObject = object []
