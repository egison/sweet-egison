module Control.Egison.Matcher.Pair
  ( Pair(..)
  , pair
  )
where

import           Data.View                      ( View(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search           ( MonadSearch )


newtype Pair a = Pair (a, a)

instance View a => View (Pair a) where
  type Target (Pair a) = (Target a, Target a)
  type Projection (Pair a) = Pair (Target a)

pair :: MonadSearch m => Pair a -> m (a, a)
pair (Pair (x, y)) = pure (x, y) `mplus` pure (y, x)
