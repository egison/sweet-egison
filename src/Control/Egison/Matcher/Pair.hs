module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  , tuple2
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch )


newtype Pair a b = Pair (a, b)

instance (Matcher a, Matcher b) => Matcher (Pair a b) where
  type Target (Pair a b) = (Target a, Target b)
  wrap (a, b) = Pair $ (wrap a, wrap b)
  unwrap (Pair (a, b)) = (unwrap a, unwrap b)

pair :: MonadSearch m => Pair a b -> m (a, b)
pair (Pair (x, y)) = pure (x, y)

tuple2 :: MonadSearch m => Pair a b -> m (a, b)
tuple2 = pair
