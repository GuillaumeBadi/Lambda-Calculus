module Evolve where

import Types
import Select
import Population
import Lambda
import Debug.Trace

evolve :: [LE] -- vars
       -> Int -- selection size
       -> Dataset -- dataset
       -> Double -- mutation rate
       -> Population -- initial population
       -> Int -- max mum iteration
       -> Int -- current iteration
       -> Double -- threshold
       -> Double -- current best score
       -> LE -- current winner
       -> IO (Population, Double, Int, LE) -- new (population, score, iteration, best)
evolve vs sz ds mr ip mi ci th bst cw
  | ci >= mi = error "max iteration reached"
  | bst >= th = return (ip, bst, ci, cw)
  | otherwise = trace ("iteration: " ++ show ci ++ ", score: " ++ (show bst) ++ "\n" ++ (show cw)) $ do
      (sl, bst', wnr) <- select vs mr (length ip) sz ds ip
      evolve vs sz ds mr sl mi (ci+1) th bst' wnr

vs = [Va "a", Va "b"]
ip = population vs 1000

true = (Ab (Va "a") (Ab (Va "b") (Va "a")))
false = (Ab (Va "a") (Ab (Va "b") (Va "b")))

dataset = [ ([true, false], false)
          , ([true, true], true)
          , ([false, false], false)
          , ([false, true], false) ]

test = do
  p <- ip
  (_, s, i, w) <- evolve
                    vs
                    2
                    dataset
                    0.7
                    p
                    10000
                    0
                    1.0
                    0.0
                    (Va "x")
  putStrLn (show s)
  putStrLn (show i)
  putStrLn (show (reduce w))
