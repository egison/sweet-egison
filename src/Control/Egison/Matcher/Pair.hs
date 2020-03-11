module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch )


newtype Pair a = Pair (a, a)

instance Matcher a => Matcher (Pair a) where
  type Target (Pair a) = (Target a, Target a)
  wrap (a, b) = Pair $ (wrap a, wrap b)
  unwrap (Pair (a, b)) = (unwrap a, unwrap b)

pair :: MonadSearch m => Pair a -> m (a, a)
pair (Pair (x, y)) = pure (x, y) `mplus` pure (y, x)
