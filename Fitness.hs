
module Fitness(fitness) where

import Types
import Debug.Trace
import Lambda

  {-
    It should work and converge slowly for
    small and easy problems 😂
    -}

fitness :: Dataset -> LE -> (Double, LE)
fitness dtst p = ((ln rghts) / (ln dtst), p)
  where f (is, o) = (call is p) == o
        rghts = filter f dtst
        ln = fromIntegral . length
