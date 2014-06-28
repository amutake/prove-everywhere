module ProveEverywhere.Types where

import Network.Wai.Handler.Warp (Port)

data Config = Config
    { configPort :: Port
    } deriving (Eq, Show)
