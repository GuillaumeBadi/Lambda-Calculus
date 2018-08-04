module Commons where

import Types
import System.Random

pk :: [a] -> IO a
pk xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

trslt :: Double -> Double -> Double -> Double -> Double -> Double
trslt mn mx cmn cmx c = (c - cmn) * (mx - mn) / (cmx - cmn) + mn

coin :: Double -> IO Bool
coin e = (randomIO :: IO Double) >>= return . (> e)

{-
  Get the maximum depth of the program.
  Useful to help calculate the probability to sect a node
  or go to the next one

  λx.x -- 1
  x -- 0
  λx.xx -- 1
-}

dpth :: LE -> Int
dpth v@Va{} = 0
dpth (Ap e e') = max (dpth e) (dpth e')
dpth (Ab _ e) = (dpth e) + 1

