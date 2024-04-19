module Maybe where

import Prelude

class Apply m <= Bind m where
  join :: forall a. m (m a) -> m a

bind :: forall a b m. Bind m => m a -> (a -> m b) -> m b
bind m f = join (f <$> m)

data Maybe a = Just a | Nothing

instance functorMaybe :: Functor Maybe where
  map fn (Just x) = Just (fn x)
  map _ _ = Nothing

instance applyMaybe :: Apply Maybe where
  apply :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
  apply (Just fn) x = fn <$> x
  apply Nothing _   = Nothing

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

instance bindMaybe :: Bind Maybe where
  join (Just x) = x
  join Nothing  = Nothing