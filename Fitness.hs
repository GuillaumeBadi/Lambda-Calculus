
module Fitness(fitness) where

import Types
import Lambda

  {-
    It should work and converge slowly for
    small and easy problems 😂
    -}

fitness :: Dataset -> LE -> (Double, LE)
fitness dtst p = ((ln rghts) / (ln dtst), p)
  where f (is, o) = (call is p) == o
        rghts = map f dtst
        ln = fromIntegral . length
