
module Parser(lc) where

import Text.Parsec
import Text.Parsec.String

import Types

-- Parse a single letter identifier
-- They don't have to be followed by a space
--   i.e. 'x'
shortIdentifier :: Parser LE
shortIdentifier = (letter >>= \c -> return $ Va (c:""))

-- Parse a long identifier
-- They should be followed by a space for readability
--   i.e. 'input '
longIdentifier :: Parser LE
longIdentifier = do
  n <- many letter
  _ <- char ' '
  return . Va $ n

-- Parse an identifier
--  i.e 'x' or 'input'
identifier = try longIdentifier <|> shortIdentifier

-- Expression Parser
--   first, it looks for an application,
--   then an abstraction definition,
--   then a single identifier
expressionP :: Parser LE
expressionP = applicationP <|> abstractionP <|> identifier

-- Abstraction Parser
abstractionP :: Parser LE
abstractionP = char 'λ' >> identifier >>= \i -> char '.' >> expressionP >>= return . Ab i

-- Parser Combinator for Parenthesis
optionalParenP :: Parser a -> Parser a
optionalParenP p = try $ between (string "(") (string ")") p <|> p

-- Application Parser
applicationP :: Parser LE
applicationP = optionalParenP $
  many1 (optionalParenP abstractionP <|> identifier) >>= \(e:es) -> return $ foldl Ap e es

-- Full Lambda calculus Parser
-- TODO: Tests
lc :: String -> (LE -> LE) -> Either ParseError LE
lc s f = parse expressionP "" s >>= (return . f)
