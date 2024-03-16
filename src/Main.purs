module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

class Apply m <= Bind m where
  join :: forall a. m (m a) -> m a

bind :: forall a b m. Bind m => m a -> (a -> m b) -> m b
bind m f = join (f <$> m)

main :: Effect Unit
main = do
  log "🍝"
