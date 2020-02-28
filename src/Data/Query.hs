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


newtype Query strategy tgt out = Query { unQuery :: tgt -> strategy out }

instance MonadSearch m => Semigroup (Query m tgt out) where
  Query a <> Query b = Query $ \x -> a x <|> b x

instance MonadSearch m => Monoid (Query m tgt out) where
  mempty = Query $ const mzero

instance MonadSearch m => Functor (Query m tgt) where
  fmap f (Query a) = Query $ fmap f . a

instance MonadSearch m => Applicative (Query m tgt) where
  pure = Query . const . pure
  liftA2 f (Query a) (Query b) = Query $ \x -> f <$> a x <*> b x

instance MonadSearch m => Alternative (Query m tgt) where
  empty = Query $ const empty
  Query a <|> Query b = Query $ \x -> a x <|> b x

instance MonadSearch m => Monad (Query m tgt) where
  Query a >>= f = Query $ \x -> a x >>= flip unQuery x . f


find :: MonadSearch m => Query m tgt out -> tgt -> m out
find (Query f) = f
