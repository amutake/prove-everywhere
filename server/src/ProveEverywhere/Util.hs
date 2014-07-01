{-# LANGUAGE OverloadedStrings #-}

module ProveEverywhere.Util where

import Control.Monad ((<=<))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Safe (readMay)

-- | 'splitCommands' splits a text including some commands to list of single command.
-- It does not support:
-- 1. bullet notation
-- 2. incomplete command
--
-- Examples:
--
-- > splitCommands "Proof. intros. induction n; auto. Qed." == ["Proof.", "intros.", "induction n; auto.", "Qed."]
-- > splitCommands "" == []
splitCommands :: Text -> [Text]
splitCommands t = map (`T.snoc` '.') (init ts) ++ [last ts]
  where
    ts = T.splitOn ". " . T.replace "\n" " " . T.strip $ t

-- | 'isNatural' checks whether the text represents natural number.
isNatural :: Text -> Bool
isNatural = isJust . (nat <=< readMay . T.unpack)
  where
    nat :: Int -> Maybe Int
    nat n | n >= 0 = Just n
          | otherwise = Nothing
