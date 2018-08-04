
module Select(select) where

import Data.List
import Data.Function

import Commons
import Types
import Population
import Fitness
import Mutation

srt = sortBy (compare `on` fst)

fill :: Double -> Int -> Population -> Population -> IO Population
fill _ z _ s | z == (length s) = return s
fill r psz ps sl = do
  mut <- coin r
  if mut
     then do
       s <- pk sl
       e <- mutate (dpth s) 0 [] s
       fill r psz ps (e:sl)
     else do
       s <- pk ps
       fill r psz ps (s:sl)

select :: Double -> Int -> Int -> Dataset -> Population -> IO Population
select r psz ssz ds ps = (fill r psz ps . map snd . take ssz . srt) $ map (fitness ds) ps
