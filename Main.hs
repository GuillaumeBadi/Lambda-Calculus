
module Main where

import Types
import Parser
import Lambda
import ChurchEncoding

rd :: String -> LE
rd s = reduce $ parseTest s

subject = "((((λx.λy.λz.xyz)(λx.xx))(λx.x))x)"

step1 = "((λx.λy.λz.xyz)(λx.xx))"
step2 = "((λy.λz.yyz)(λx.x))"
step3 = "((λz.z)x)"
step4 = "(((λx.x)(λx.x)(λx.x)))"

{- main = (putStrLn . show . rd) step2 -}
main = (putStrLn . show . reduceWith [Va "x"] . parseTest) subject
