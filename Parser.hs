
module Parser(lc) where

import Text.Parsec
import Text.Parsec.String

import Types

stIde :: Parser LE
stIde = (letter >>= \c -> return $ Va (c:""))

lgIde :: Parser LE
lgIde = do
  n <- many letter
  _ <- char ' '
  return . Va $ n

ide = try lgIde <|> stIde

expr :: Parser LE
expr = appP <|> absP <|> ide

absP :: Parser LE
absP = char 'λ' >> ide >>= \i -> char '.' >> expr >>= return . Ab i

optPar :: Parser a -> Parser a
optPar p = try $ between (string "(") (string ")") p <|> p

appP :: Parser LE
appP = optPar $ many1 (optPar absP <|> ide) >>= \(e:es) -> return $ foldl Ap e es

  {-
    TODO:
      Find a more elegant way than the case-of statement
    -}

lc :: String -> (LE -> LE) -> LE
lc s f = case parse expr "" s of
          Left err -> error (show err)
          Right e -> f e
