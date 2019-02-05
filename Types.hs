module Types where

data LE = Va String | Ap LE LE | Ab LE LE
  deriving (Eq)

data DB =  DBI Int String | DBAp DB DB | DBAb String DB | DBNil
  deriving (Eq)

type Population = [LE]
type Dataset = [([LE], LE)]

isdbab :: DB -> Bool
isdbab a@DBAb{} = True
isdbab _ = False

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

