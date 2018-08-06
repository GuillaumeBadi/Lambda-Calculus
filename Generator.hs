
module Generator(gen) where

import System.Random
import Debug.Trace
import Data.Char

import Types
import Commons
import Lambda

frvs :: [LE] -> LE -> LE
frvs vs p = foldr Ab p vs

nxtc :: Char -> Char
nxtc c = chr (ord c + 1)

ab [] = error "available variables should not be empty"
-- ab [] = do
  -- g <- gen [(Va "a")]
  -- return $ Ab (Va "a") g

ab vs | (length vs) == 26 = va vs

ab vs@((Va (n:_)):_) = do
  let c = nxtc n
  g <- gen_ (Va (c:""):vs)
  return $ Ab (Va (c:"")) g
  
va [] = error "available variables should not be empty"
va vs = pk vs >>= return

ap vs = gen_ vs >>= \e1 -> gen_ vs >>= \e2 -> return $ Ap e1 e2

gen_ :: [LE] -> IO LE
gen_ vs = do
  term <- pk [0, 1, 2]
  case term of
      -- Va
      0 -> va vs
      -- Ap
      1 -> ap vs
      -- Ab
      2 -> ab vs

gen :: [LE] -> IO LE
gen vs = gen_ [Va "a"] >>= return . (Ab (Va "a"))
