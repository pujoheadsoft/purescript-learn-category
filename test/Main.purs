module Test.Main where

import Prelude

import Data.Array (head)
import Data.String (null)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] $ 
  describe "Array関手からMaybe関手への自然変換は可換である" do
    let
      nt_map = head <<< map null
      map_nt = map null <<< head
    it "値がない場合" do
      nt_map [""] `shouldEqual` map_nt [""]

    it "値がある場合" do
      nt_map ["any"] `shouldEqual` map_nt ["any"]


