module Lambda where

import Data.List (elemIndex)

import Text.Parsec
import Text.Parsec.String

data LE = Va String | Ap LE LE | Ab LE LE
  deriving (Eq)

data DB =  DBI Int String | DBAp DB DB | DBAb String DB | DBNil

dbi :: [LE] -> LE -> DB
dbi s (Ap e e') = (DBAp (dbi s e) (dbi s e'))
dbi s (Ab v@(Va n) e) = DBAb n (dbi (v:s) e)
dbi s v@(Va n) = case elemIndex v s of
                    Nothing -> error $ "Unknown variable: " ++ n
                    Just i -> DBI i n

rpl i (DBAb v e) = DBAb v (rpl (i+1) e)
rpl i (DBAp e e') = DBAp (rpl i e) (rpl i e')
rpl i e@(DBI i' _)
  | i == i' = DBNil
  | otherwise = e

dec :: DB -> DB
dec (DBI i n) = DBI (i-1) n
dec (DBAb v e) = DBAb v (dec e)
dec (DBAp e e') = DBAp (dec e) (dec e')
dec e = e

ins :: DB -> DB -> DB
ins e DBNil = e
ins e d@DBI{} = d
ins e (DBAb v e') = DBAb v (ins e e')
ins e (DBAp e' e'') = DBAp (ins e e') (ins e e'')

sub :: DB -> DB -> DB
sub (DBAb v e) e' = ins e' $ dec $ rpl 0 e
sub _ _= error "Can only subsitute abstractions with expression"

rdc :: DB -> DB
rdc i@DBI{} = i
rdc (DBAb v e) = (DBAb v . rdc) e
rdc (DBAp i@DBI{} e) = DBAp i (rdc e)
rdc (DBAp e@DBAb{} e') = rdc $ sub e (rdc e')
rdc (DBAp e e') = rdc $ DBAp (rdc e) (rdc e')

toLE :: DB -> LE
toLE (DBI i n) = Va n
toLE (DBAp e e') = Ap (toLE e) (toLE e')
toLE (DBAb v e) = Ab (Va v) (toLE e)
toLE _ = error "Should not be Nil"

lc :: String -> LE
lc s = case parse expr "" s of
        Left err -> error (show err)
        Right e -> (toLE . rdc . dbi []) e
  
instance Show DB where
  show (DBI i _) = show i
  show (DBAp v@DBI{} v'@DBI{}) = show v ++ show v'
  show (DBAp e v'@DBI{}) = "(" ++ show e ++ ")" ++ show v'
  show (DBAp v'@DBI{} e) =  show v' ++ "(" ++ show e ++ ")"
  show (DBAp e e') = "(" ++ show e ++ ")(" ++ show e' ++ ")"
  show (DBAb _ e) = "λ." ++ show e
  show DBNil = "●"

instance Show LE where
  show (Va n) = n
  show (Ap v@Va{} v'@Va{}) = show v ++ show v'
  show (Ap e v'@Va{}) = "(" ++ show e ++ ")" ++ show v'
  show (Ap v'@Va{} e) =  show v' ++ "(" ++ show e ++ ")"
  show (Ap e e') = "(" ++ show e ++ ")(" ++ show e' ++ ")"
  show (Ab v e) = "λ" ++ show v ++ "." ++ show e

ide :: Parser LE
ide = letter >>= \c -> return $ Va (c:"")

dt :: Parser Char
dt = char '.'

expr :: Parser LE
expr = appP <|> absP <|> ide

absP :: Parser LE
absP = char 'λ' >> ide >>= \i -> char '.' >> expr >>= return . Ab i

optPar :: Parser a -> Parser a
optPar p = try $ between (string "(") (string ")") p <|> p

appP :: Parser LE
appP = optPar $ many1 (optPar absP <|> ide) >>= \(e:es) -> return $ foldl Ap e es
