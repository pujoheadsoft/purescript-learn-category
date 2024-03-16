module Test.Main where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Data.String (length, null)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Categoryは以下の条件を満たす" do
    let 
      f = isNaN
      g = show
      h = length
      a = 100.0
    it "結合律を満たす" do
      ((h <<< g) <<< f) a `shouldEqual` (h <<< (g <<< f)) a

    it "左単位律を満たす" do
      (identity <<< f) a `shouldEqual` f a

    it "右単位律を満たす" do
      f a `shouldEqual` (f <<< identity) a


  describe "Maybe NumberはFunctor則を満たす" do
    it "射の合成を保つ" do
      let 
        a = Just 1.0
        f = isNaN
        g = show
      (map (g <<< f) a) `shouldEqual` ((map g <<< map f) a)
    
    it "恒等射を保つ" do
      let a = Just 1.0
      map identity a `shouldEqual` identity a

  describe "自然変換は可換である" do
    let
      nt_map = head <<< map null -- mapした後に自然変換
      map_nt = map null <<< head -- 自然変換した後にmap
    it "先頭が空文字の場合" do
      nt_map ["", "any"] `shouldEqual` map_nt ["", "any"]

    it "先頭が空文字でない場合" do
      nt_map ["any", ""] `shouldEqual` map_nt ["any", ""]

  describe "Array Intはモナドの条件を満たす" do
    it "結合律を満たす" do
      let 
        t3 = [
          [
            [1, 2], 
            [3, 4]
          ], [
            [5, 6],
            [7, 8]
          ]
        ]
      (join <<< map join) t3 `shouldEqual` (join <<< join) t3

    it "単位元律を満たす" do
      let
        ta = [10, 20, 30]
      (join <<< pure) ta `shouldEqual` identity ta
      (join <<< map pure) ta `shouldEqual` identity ta

  describe "Maybeはモナド則を満たす" do
    it "左単位元律を満たす" do
      -- `pure a >>= h = h a`
      let h = Just
      (pure 10 >>= h) `shouldEqual` h 10
    
    it "右単位元律を満たす" do
      -- `m >>= pure = m`
      let m = Just 10
      (m >>= pure) `shouldEqual` m
    
    it "結合律を満たす" do
      -- (m >>= g) >>= h = m >>= (\x -> g x >>= h)
      let
        m = Just 1
        g = \x -> pure (x + 10)
        h = \x -> pure (show x)
      ((m >>= g) >>= h) `shouldEqual` (m >>= (\x -> g x >>= h))

  describe "Maybeはモナド則を満たす(join版)" do
    it "左単位元律を満たす" do
      let h = Just
      (join <<< map h) (pure 10) `shouldEqual` h 10
    
    it "右単位元律を満たす" do
      let m = Just 10
      (join <<< map pure) m `shouldEqual` m
    
    it "結合律を満たす" do
      let
        m = Just 1
        g = \x -> pure (x + 10)
        h = \x -> pure (show x)
      ((join <<< map h) <<< (join <<< map g)) m `shouldEqual` (join <<< map (\x -> (join <<< map h) (g x))) m

