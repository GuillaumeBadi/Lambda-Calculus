module Population(population) where

import Control.Monad

import Types
import Generator

population :: Int -> IO [LE]
population sz = replicateM sz (gen [])
