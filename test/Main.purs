module Test.Main where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  -- describe "Categoryは以下の条件を満たす" do
  --   let 
  --     f = isNaN
  --     g = show
  --     h = length
  --     a = 100.0
  --   it "結合律を満たす" do
  --     ((h <<< g) <<< f) a `shouldEqual` (h <<< (g <<< f)) a

  --   it "左単位律を満たす" do
  --     (identity <<< f) a `shouldEqual` f a

  --   it "右単位律を満たす" do
  --     f a `shouldEqual` (f <<< identity) a


  -- describe "Maybe NumberはFunctor則を満たす" do
  --   it "射の合成を保つ" do
  --     let 
  --       a = Just 1.0
  --       f = isNaN
  --       g = show
  --     (map (g <<< f) a) `shouldEqual` ((map g <<< map f) a)
    
  --   it "恒等射を保つ" do
  --     let a = Just 1.0
  --     map identity a `shouldEqual` identity a

  describe "自然変換は可換である" do
    let
      nt_map = head <<< map null -- mapした後に自然変換
      map_nt = map null <<< head -- 自然変換した後にmap
    it "先頭が空文字の場合" do
      nt_map ["", "any"] `shouldEqual` map_nt ["", "any"]

    it "先頭が空文字でない場合" do
      nt_map ["any", ""] `shouldEqual` map_nt ["any", ""]

  describe "Array IntはMonad則を満たす" do
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
        {-
          Aは対象
          fは関数
          関手Tは対象関数と射関数からなる
          T(A)は対象関数で写した対象
          T(f)は射関数(map)で写した関数
          Aを配列[]とする。
          T(A)は配列の配列[[]]となる
          Aが配列なのでfは配列に対しての関数となる
          T(f)は map 配列の関数 となる。

          μはjoin
          μ_Aは配列のjoin
          T(μ_A)は、μ_Aが関数なので射関数になり、(map 配列のjoin)となる。

          T(A)は配列の配列[[]]だったので、
          μ_T(A)は、配列の配列のjoin
        -}
        μ = join
        tμ_a = map μ
        μ_ta = μ
      (μ <<< tμ_a) t3 `shouldEqual` (μ <<< μ_ta) t3

