
module Select(select) where

import Data.List
import Debug.Trace
import Data.Function

import Commons
import Types
import Population
import Fitness
import Mutation

srt = reverse . sortBy (compare `on` fst)

fill :: [LE] -> Double -> Int -> Population -> Population -> IO Population
fill _ _ z _ s | z == (length s) = return s
fill vs r psz ps sl = do
  mut <- coin r
  if mut
     then do
       s <- pk sl
       e <- mutate (dpth s) 0 vs s
       fill vs r psz ps (e:sl)
     else do
       s <- pk ps
       fill vs r psz ps (s:sl)

select :: [LE] -> Double -> Int -> Int -> Dataset -> Population -> IO (Population, Double, LE)
select vs r psz ssz ds ps = do
  n <- np
  trace ("Best fitness test: " ++ show (fitness ds wnr)) $ return (n, bst, wnr)
  where np = (fill vs r psz ps . map snd . take ssz) srtd
        scrd = map (fitness ds) ps
        srtd = srt scrd
        bst = fst (scrd !! 0)
        wnr = snd (scrd !! 0)
