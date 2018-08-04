module Lambda(reduce, dbi, rdc, toLE) where

import Debug.Trace
import Data.List (elemIndex)

import Types

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
rdc (DBAp e e') = DBAp (rdc e) (rdc e')

toLE :: DB -> LE
toLE (DBI i n) = Va n
toLE (DBAp e e') = Ap (toLE e) (toLE e')
toLE (DBAb v e) = Ab (Va v) (toLE e)
toLE _ = error "Should not be Nil"

reduce :: LE -> LE
reduce = (toLE . rdc . dbi [])

call :: [LE] -> LE -> LE
call = undefined
