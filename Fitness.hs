
module Fitness(fitness) where

import Types

cvrg :: [([LE], LE)] -> (LE -> Double)
cvrg dtst p = map

fitness :: [(String, LE)] -- list of parameters
        -> (LE -> LE) -- the reducer function
        -> (LE -> Double) -- function evaluating the generated program
        -> LE -- The generated program
        -> Double -- final score

fitness ps rdc ev pr = undefined
  where pr' = 
        
