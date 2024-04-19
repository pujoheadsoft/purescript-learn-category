module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

xxx :: Maybe String
xxx = do
  pure "x"
    >>= \_ -> Nothing
    >>= \y -> pure (y <> "z")

aaa :: Maybe String
aaa = do
  pure "x"
    >>= \x -> pure (x <> "y")
    >>= \y -> pure (y <> "z")

-- bind :: forall a b m. Bind m => m a -> (a -> m b) -> m b
-- bind m f = join (f <$> m)

xx2 :: Maybe String
xx2 = let
  -- Just "x" が Just Nothingになる
  a1 = ((\_ -> Nothing) <$> pure "x")
  -- Just Nothing が Nothingになる
  a2 = join a1
  -- NothingのmapなのでNothingのまま
  a3 = ((\y -> pure $ y <> "z") <$> a2)
  -- そのままNothingになる
  in join a3

xx3 :: Maybe String
xx3 = let
  -- Just "x" が Just (Just "xy")になる
  a1 = ((\x -> pure $ x <> "y") <$> pure "x")
  -- Just (Just "xy") が Just "xy"になる
  a2 = join a1
  -- Just "xy" が Just (Just "xyz")になる
  a3 = ((\y -> pure $ y <> "z") <$> a2)
  -- Just (Just "xyz") が Just "xyz"になる
  in join a3

xx4 :: Maybe String
xx4 = let
  -- Just "x" が Just "xy"になる
  a1 = ((\x -> x <> "y") <$> pure "x")
  -- Just "xy" が Just "xyz"になる
  a3 = ((\y -> y <> "z") <$> a1)
  in a3

xx5:: Maybe String
xx5 = let
  -- 関数の結果をNothingにしたいとする
  a1 = ((\x -> Nothing) <$> pure "x")
  -- しかしa1はMaybe Maybeになっているので、joinなしでこういうmapを呼べない。
  -- a3 = ((\y -> y <> "z") <$> a1)
  -- こうやってMaybeを処理するような関数をmapに渡せば呼べるが
  -- この処理の中でNothingを返すとMaybe Maybeになるのでjoinが必要
  a3 = (case _ of
          (Just x) -> x <> "z"
          _ -> "")
       <$> a1
  in a3

{-
  失敗するかもしれない処理をmapの中でやると入れ子になる、
  それを剥がす処理がjoin
  これのおかげで失敗するかもしれない処理を率直に書きつつ、処理を継続できる。
  抽象的に考えると、monadであることを保ち続けたまま、mapしていくのに必要ということか。
-}

main :: Effect Unit
main = do
  log "🍝"
  logShow xxx
  logShow aaa
  logShow xx2
  logShow xx3
  logShow xx4

