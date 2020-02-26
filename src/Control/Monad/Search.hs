module Control.Monad.Search
  ( MonadSearch(..)
  , BFS
  , DFS
  )
where

import           Data.Foldable                  ( toList )
import           Control.Applicative            ( Alternative(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Logic            ( MonadLogic(..)
                                                , Logic
                                                , observeAll
                                                )


-- | MonadSearch. Represents searches with backtracking.
class MonadPlus m => MonadSearch m where
  collect :: m a -> [a]

  (>->) :: m a -> (a -> m b) -> m b
  exclude :: m a -> m ()

  default collect :: Foldable m => m a -> [a]
  collect = toList

-- | Interleaving BFS Monad
newtype BFS a = BFS { unBFS :: Logic a }
  deriving newtype (Functor, Foldable, Applicative, Alternative, Monad, MonadPlus)

instance MonadSearch BFS where
  (BFS m) >-> f = BFS (m >>- (unBFS . f))
  exclude = BFS . lnot . unBFS
  collect = observeAll . unBFS

-- | DFS Monad
newtype DFS a = DFS { unDFS :: Logic a }
  deriving newtype (Functor, Foldable, Applicative, Alternative, Monad, MonadPlus)

instance MonadSearch DFS where
  (DFS m) >-> f = DFS (m >>= (unDFS . f))
  exclude = DFS . lnot . unDFS
  collect = observeAll . unDFS

-- | You can also implement 'MonadSearch' with list ;)
-- This instance naively performs DFS.
instance MonadSearch [] where
  (>->) = (>>=)
  exclude []      = pure ()
  exclude (_ : _) = mzero
