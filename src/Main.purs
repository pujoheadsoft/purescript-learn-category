module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number (isNaN)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)

class Apply m <= Bind m where
  join :: forall a. m (m a) -> m a

type NaturalTransformation :: forall k. (k -> Type) -> (k -> Type) -> Type
type NaturalTransformation f g = forall a. f a -> g a

infixr 4 type NaturalTransformation as ~>

bind :: forall a b m. Bind m => m a -> (a -> m b) -> m b
bind m f = join (f <$> m)

xx :: Function String String
xx = show

yy :: Maybe String
yy = show <$> isNaN <$> (Just 1.0)

zz :: Maybe String
zz = map (show <<< isNaN) (Just 1.0)

aa :: Maybe String
aa = (map show <<< map isNaN) (Just 1.0)

main :: Effect Unit
main = do
  logShow (eq yy aa)
  log "🍝"
