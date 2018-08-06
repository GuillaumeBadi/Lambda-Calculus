module Population(population) where

import Control.Monad

import Types
import Generator

population :: [LE] -> Int -> IO [LE]
population vs sz = replicateM sz (gen vs)
