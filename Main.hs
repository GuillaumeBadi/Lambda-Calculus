
module Main where

import Types
import Parser
import Lambda

rd :: String -> LE
rd s = lc s reduce

main = undefined
