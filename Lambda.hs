module Lambda(reduce, reduceWith, reduce2, dbi, reduceDBI, toLE, call) where

import Data.List (elemIndex)

import Types

-- Changes a regular λ calculus with dbis to avoid
--   name collapse while reducing the expression
dbi :: [LE] -- A list of free variables (i.e. inputs)
    -> LE -- An expression
    -> DB -- The expression translated into DeBruijn Index
dbi s (Ap e e') = (DBAp (dbi s e) (dbi s e'))
dbi s (Ab v@(Va n) e) = DBAb n (dbi (v:s) e)
dbi s v@(Va n) = case elemIndex v s of
                   Nothing -> error ("DBI: Unknown variable: " ++ n ++ ", variables: " ++ (show s))
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

substitute :: DB -> DB -> DB
substitute (DBAb v e) e' = ins e' $ dec $ rpl 0 e
substitute _ _= error "Can only subsitute abstractions with expression"

-- Reduces a DBI expression to its minimal form
reduceDBI :: DB -> DB
reduceDBI i@DBI{} = i
reduceDBI (DBAb v e) = (DBAb v . reduceDBI) e
reduceDBI (DBAp i@DBI{} e) = DBAp i (reduceDBI e)
reduceDBI (DBAp e@DBAb{} e') = reduceDBI $ substitute e (reduceDBI e')

reduceDBI (DBAp e e')
  | isdbab ne = reduceDBI $ DBAp ne ne'
  | otherwise = DBAp ne ne'
  where ne = reduceDBI e
        ne' = reduceDBI e'

toLE :: DB -> LE
toLE (DBI i n) = Va n
toLE (DBAp e e') = Ap (toLE e) (toLE e')
toLE (DBAb v e) = Ab (Va v) (toLE e)
toLE _ = error "Should not be Nil"

-- reduce2 :: LE -> LE
reduce2 = (reduceDBI . dbi [])

reduceWith vars = (toLE . reduceDBI . dbi vars)

reduce :: LE -> LE
reduce = (toLE . reduceDBI . dbi [])

call :: [LE] -> LE -> LE
call vs e = reduce $ foldl Ap e vs
