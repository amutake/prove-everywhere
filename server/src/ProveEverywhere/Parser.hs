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

-- | Prompt parser
prompt :: Parser CoqtopState
prompt = between (symbol "<prompt>") (symbol "</prompt>") internal

internal :: Parser CoqtopState
internal = do
    current <- token theorem
    _ <- symbol "<"
    wholeState <- natural
    stack <- token theoremStack
    theoremState <- natural
    _ <- symbol "<"
    return CoqtopState
        { promptCurrentTheorem = current
        , promptWholeStateNumber = wholeState
        , promptTheoremStack = stack
        , promptTheoremStateNumber = theoremState
        }

-- | Parser for theorem name
theorem :: Parser Text
theorem = T.pack <$> some (alphaNum <|> oneOf "_'")

-- | Parses |theorem1|theorem2| ... |theoremn| and return list of theorem names
theoremStack :: Parser [Text]
theoremStack = between (char '|') (char '|') $ sepBy' theorem (char '|')
  where
    sepBy' p sep = sepBy1' p sep <|> pure []
    sepBy1' p sep = (:) <$> p <*> many (try (sep *> p))
