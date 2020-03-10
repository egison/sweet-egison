{-# LANGUAGE DerivingVia #-}

module Control.Monad.Search
  ( MonadSearch(..)
  , BFS
  , DFS
  )
where

import           GHC.Generics                   ( (:.:)(..) )
import           Data.Foldable                  ( toList )
import           Control.Applicative            ( Alternative(..) )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad.Trans.Maybe      ( MaybeT(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Logic            ( MonadLogic(..)
                                                , Logic
                                                )


-- | MonadSearch. Represents searches with backtracking.
class MonadPlus m => MonadSearch m where
  collect :: m a -> [a]
  guarded :: Bool -> m a -> m a

  (>->) :: m a -> (a -> m b) -> m b
  exclude :: m a -> m ()

  default collect :: Foldable m => m a -> [a]
  collect = toList


-- newtype wrapper to make @'MonadLogic' m => 'MonadLogic' ('MaybeT' m)@ instance that skips 'Nothing' during the search.
newtype SkipNothing m a = SkipNothing (MaybeT m a)
  deriving newtype MonadTrans
  deriving newtype (Functor, Applicative, Monad)
  deriving Alternative via m :.: Maybe

instance (Monad m, Alternative m) => MonadPlus (SkipNothing m)

instance MonadLogic m => MonadLogic (SkipNothing m) where
  msplit (SkipNothing m) = (lift . msplit $ runMaybeT m) >>= \case
    Just (Just x , m') -> pure $ Just (x, SkipNothing $ MaybeT m')
    Just (Nothing, m') -> msplit . SkipNothing $ MaybeT m'
    Nothing            -> pure Nothing

-- | Interleaving BFS Monad
newtype BFS a = BFS { unBFS :: MaybeT Logic a }
  deriving newtype (Functor, Applicative, Monad, Foldable)
  deriving Alternative via Logic :.: Maybe
  deriving MonadLogic via SkipNothing Logic

instance MonadPlus BFS

instance MonadSearch BFS where
  guarded True  m = m
  guarded False _ = BFS . MaybeT $ pure Nothing
  exclude = lnot
  BFS m >-> f = BFS . MaybeT $ runMaybeT m >>- \case
    Just x  -> runMaybeT . unBFS $ f x
    Nothing -> pure Nothing

-- | DFS Monad
newtype DFS a = DFS { unDFS :: MaybeT [] a }
  deriving newtype (Functor, Applicative, Monad, Foldable)
  deriving Alternative via [] :.: Maybe
  deriving MonadLogic via SkipNothing []

instance MonadPlus DFS

instance MonadSearch DFS where
  guarded True  m = m
  guarded False _ = DFS . MaybeT $ pure Nothing
  exclude = lnot
  DFS m >-> f = DFS . MaybeT $ runMaybeT m >>= \case
    Just x  -> runMaybeT . unDFS $ f x
    Nothing -> pure Nothing
