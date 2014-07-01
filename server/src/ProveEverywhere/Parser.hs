module ProveEverywhere.Parser (parsePrompt) where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parsec (parse, ParseError)
import Text.Parsec.Text (Parser)

import ProveEverywhere.Types

parsePrompt :: Text -> Either ParseError CoqtopState
parsePrompt = parse prompt "prompt"

prompt :: Parser CoqtopState
prompt = between (symbol "<prompt>") (symbol "</prompt>") internal

internal :: Parser CoqtopState
internal = do
    current <- theorem
    _ <- symbol "<"
    wholeState <- natural
    stack <- between (symbol "|") (symbol "|") $
        sepBy theorem (symbol "|")
    theoremState <- natural
    _ <- symbol "<"
    return CoqtopState
        { promptCurrentTheorem = current
        , promptWholeStateNumber = wholeState
        , promptTheoremStack = stack
        , promptTheoremStateNumber = theoremState
        }

theorem :: Parser Text
theorem = token $ T.pack <$> some (letter <|> oneOf "_")
