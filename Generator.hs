
module Generator(gen) where

import System.Random
import Data.Char

import Types
import Commons
import Lambda

frvs :: [LE] -> LE -> LE
frvs vs p = foldr Ab p vs

nxtc :: Char -> Char
nxtc c = chr (ord c + 1)

ab [] = do
  g <- gen [(Va "a")]
  return $ Ab (Va "a") g

ab vs | (length vs) == 26 = va vs

ab vs@((Va (n:_)):_) = do
  let c = nxtc n
  g <- gen (Va (c:""):vs)
  return $ Ab (Va (c:"")) g
  
va [] = ab []
va vs = pk vs >>= return

ap vs = gen vs >>= \e1 -> gen vs >>= \e2 -> return $ Ap e1 e2

gen :: [LE] -> IO LE
gen vs = do
  term <- pk [0, 1, 2]
  fmap (frvs vs) $
    case term of
      -- Va
      0 -> va vs
      -- Ap
      1 -> ap vs
      -- Ab
      2 -> ab vs
