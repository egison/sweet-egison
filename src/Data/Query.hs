module Data.Query
  ( Query(..)
  , find
  )
where

import           Control.Monad                  ( mzero )
import           Control.Applicative            ( Alternative(..)
                                                , Applicative(..)
                                                )
import           Control.Monad.Search           ( MonadSearch )
import           Data.Tagged                    ( Tagged(..) )


newtype Query strategy tag tgt out = Query { unQuery :: Tagged tag tgt -> strategy out }

instance MonadSearch m => Semigroup (Query m tag tgt out) where
  {-# INLINE (<>) #-}
  Query a <> Query b = Query $ \x -> a x <|> b x

instance MonadSearch m => Monoid (Query m tag tgt out) where
  {-# INLINABLE mempty #-}
  mempty = Query $ const mzero

instance MonadSearch m => Functor (Query m tag tgt) where
  {-# INLINABLE fmap #-}
  fmap f (Query a) = Query $ fmap f . a

instance MonadSearch m => Applicative (Query m tag tgt) where
  {-# INLINABLE pure #-}
  pure = Query . const . pure
  {-# INLINABLE liftA2 #-}
  liftA2 f (Query a) (Query b) = Query $ \x -> f <$> a x <*> b x

instance MonadSearch m => Alternative (Query m tag tgt) where
  {-# INLINABLE empty #-}
  empty = Query $ const empty
  {-# INLINE (<|>) #-}
  Query a <|> Query b = Query $ \x -> a x <|> b x

instance MonadSearch m => Monad (Query m tag tgt) where
  {-# INLINABLE (>>=) #-}
  Query a >>= f = Query $ \x -> a x >>= flip unQuery x . f


{-# INLINE find #-}
find
  :: forall m tag tgt out
   . MonadSearch m
  => Query m tag tgt out
  -> tgt
  -> m out
find (Query f) tgt = f $ Tagged tgt
