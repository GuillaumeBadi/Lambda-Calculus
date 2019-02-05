module Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Types
import Parser
import Lambda

x = (Va "x")
y = (Va "y")
c = (Va "c")
lid = (Ab x x)

main :: IO ()
main = hspec $ do
  describe "Parser" $ do
    it "parses x" $ do
      (parseTest "x") `shouldBe` x
    it "parses xyc" $ do
      (parseTest "xyc") `shouldBe` (Ap (Ap x y) c)
    it "parses λx.x" $ do
      (parseTest "λx.x") `shouldBe` lid
    it "parses (λx.x)" $ do
      (parseTest "(λx.x)") `shouldBe` lid
    it "parses (λx.λy.xy)" $ do
      (parseTest "(λx.λy.xy)") `shouldBe` (Ab x (Ab y (Ap x y)))
    it "parses (λx.λy.x(λc.c))" $ do
      (parseTest "(λx.λy.x(λc.c))") `shouldBe` (Ab x (Ab y (Ap x (Ab c c))))
    it "parses (λy.((λx.xy)(λx.xy)))" $ do
      (parseTest "(λy.((λx.xy)(λx.xy)))") `shouldBe` (Ab y (Ap (Ab x (Ap x y)) (Ab x (Ap x y))))
    it "parses (λx.λy.x(λc.c))" $ do
      (parseTest "(λx.λy.x(λc.c))") `shouldBe` (Ab x (Ab y (Ap x (Ab c c))))
    it "parses (λx.xx)(λy.y)" $ do
      (parseTest "((λx.xx)(λy.y))") `shouldBe` (Ap (Ab x (Ap x x)) (Ab y y))
    it "parses (((λx.λy.x(λc.c))))" $ do
      pendingWith "Not ready yet 😂"
    it "parses ((λx.λy.λz.xyz)(λx.xx)(λx.x)x)" $ do
      (parseTest "((λx.λy.λz.xyz)(λx.xx)(λx.x)x)") `shouldBe` x

  describe "DBI" $ do
    it "dbi λx.x" $ do
      (dbi [] $ parseTest "λx.x") `shouldBe` (DBAb "x" (DBI 0 "x"))
    it "dbi λx.xy" $ do
      (dbi [y] $ parseTest "λx.xy") `shouldBe` (DBAb "x" (DBAp (DBI 0 "x") (DBI 1 "y")))

  describe "Reducer" $ do
    it "reduces λx.x -> λx.x" $ do
      (reduce $ parseTest "λx.x") `shouldBe` lid
    it "reduces (λx.(λx.x)x) -> λx.x" $ do
      (reduce $ parseTest "(λx.(λx.x)x)") `shouldBe` lid
    it "reduces (λy.((λx.xy)(λx.xy))) -> λy.λx.x(λx.y)" $ do
      (reduce $ parseTest "(λy.((λx.xy)(λx.xy)))") `shouldBe` (Ab y (Ab x (Ap x (Ab x y))))
