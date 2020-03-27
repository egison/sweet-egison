{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveAnyClass #-}

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
  guarded :: m () -> m a -> m a

  (>->) :: m a -> (a -> m b) -> m b
  exclude :: m a -> m ()

  {-# INLINABLE collect #-}
  default collect :: Foldable m => m a -> [a]
  collect = toList


-- newtype wrapper to make @'MonadLogic' m => 'MonadLogic' ('MaybeT' m)@ instance that skips 'Nothing' during the search.
newtype SkipNothing m a = SkipNothing (MaybeT m a)
  deriving newtype MonadTrans
  deriving newtype (Functor, Applicative, Monad)
  deriving Alternative via m :.: Maybe
  deriving anyclass MonadPlus

instance MonadLogic m => MonadLogic (SkipNothing m) where
  msplit (SkipNothing m) = (lift . msplit $ runMaybeT m) >>= \case
    Just (Just x , m') -> pure $ Just (x, SkipNothing $ MaybeT m')
    Just (Nothing, m') -> msplit . SkipNothing $ MaybeT m'
    Nothing            -> pure Nothing

-- | Interleaving BFS Monad
newtype BFS a = BFS { unBFS :: MaybeT Logic a }
  deriving newtype (Functor, Applicative, Monad, Foldable)
  deriving Alternative via Logic :.: Maybe
  deriving anyclass MonadPlus
  deriving MonadLogic via SkipNothing Logic

instance MonadSearch BFS where
  {-# INLINABLE guarded #-}
  guarded m x = ifte m (const x) (BFS . MaybeT $ pure Nothing)
  {-# INLINABLE exclude #-}
  exclude = lnot
  {-# INLINE (>->) #-}
  BFS m >-> f = BFS . MaybeT $ runMaybeT m >>- \case
    Just x  -> runMaybeT . unBFS $ f x
    Nothing -> pure Nothing

-- | DFS Monad
newtype DFS a = DFS { unDFS :: [a] }
  deriving newtype (Functor, Applicative, Monad, Foldable, Alternative, MonadPlus, MonadLogic)

instance MonadSearch DFS where
  {-# INLINABLE guarded #-}
  guarded m x = ifte m (const x) mzero
  {-# INLINABLE exclude #-}
  exclude = lnot
  {-# INLINE (>->) #-}
  DFS m >-> f = DFS $ m >>= (unDFS . f)
