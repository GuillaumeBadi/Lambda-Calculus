
module Main where

import Text.Parsec

import Types
import Parser
import Lambda
import ChurchEncoding

rd :: String -> Either ParseError LE
rd s = lc s reduce

main = (putStrLn . show . rd) "((λx.x)(λy.λz.zy))"
