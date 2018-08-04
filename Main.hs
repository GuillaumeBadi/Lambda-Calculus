
module Main where

import Types
import Lambda
import ChurchEncoding

rd :: String -> LE
rd s = lc s reduce

main = undefined
