module ProveEverywhere.Types where

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
