
module Parser(lc, parseTest) where

import Text.Parsec hiding (parseTest)
import Text.Parsec.String

import Types

-- Parse a single letter identifier
-- They don't have to be followed by a space
--   i.e. 'x'
shortIdentifier :: Parser LE
shortIdentifier = letter >>= return . Va . (:"")

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
{- identifier = try longIdentifier <|> shortIdentifier -}
identifier = shortIdentifier

-- Expression Parser
--   first, it looks for an application,
--   then an abstraction definition,
--   then a single identifier
expressionP :: Parser LE
expressionP = abstractionP <|> do aps <- many1 termP
                                  return $ foldl1 Ap aps


termP :: Parser LE
termP = identifier <|> do char '('
                          exp <- expressionP
                          char ')'
                          return exp


-- Abstraction Parser
abstractionP :: Parser LE
abstractionP = do char 'λ'
                  i <- identifier
                  char '.'
                  exp <- expressionP
                  return $ Ab i exp

-- Parser Combinator for Parenthesis
optionalParenP :: Parser a -> Parser a
optionalParenP p = try $ between (string "(") (string ")") p <|> p

-- Application Parser
applicationP :: Parser LE
applicationP = optionalParenP $
  many1 (optionalParenP (abstractionP <|> identifier <|> applicationP)) >>= \(e:es) -> return $ foldl Ap e es

-- Full Lambda calculus Parser
lc :: String -> Either ParseError LE
lc s = parse expressionP "" s

parseTest :: String -> LE
parseTest s = case parse expressionP "" s of
                 Left e -> error (show e)
                 Right l -> l
