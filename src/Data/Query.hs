module Data.Query
  ( Query(..)
  , SearchStrategy
  , query
  )
where

import           Control.Monad                  ( mzero )
import           Control.Applicative            ( Alternative(..)
                                                , Applicative(..)
                                                )
import           Control.Monad.Search           ( MonadSearch
                                                , BFS
                                                , DFS
                                                )
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


-- | Class that represents search strategies used in pattern matching.
-- In the view of implementors, this is a helper class to default search strategy to BFS in 'query'.
--
-- When the strategy is explicitly specified like @'query' \@_ \@BFS@, this class appears as @'SearchStrategy' 'BFS'@ in the context. This constraint matches to @'SearchStrategy' a@ and @'SearchStrategy' 'BFS'@ instances. The latter is more specific so would be chosen. @'SearchStrategy' 'DFS'@ also typechecks in the similar way.
--
-- When the strategy is not specified, the parameter is left uninstantiated as @'SearchStrategy' a0@. This constraint only matches to @'SearchStrategy' a@ instance and GHC will choose it since it is @INCOHERENT@. Once the instance is chosen, the constraint @a ~ 'BFS'@ is introduced and @a@ is defaulted to 'BFS'. @INCOHERENT@ is necessary for GHC to cut short in the decision. Without @INCOHERENT@, GHC looks for further instances that /unify/ with @SearchStrategy a0@ but not /match/ it and fails because of them.
--
-- For details, see [the relevant section in GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances).
class MonadSearch m => SearchStrategy m
instance {-# INCOHERENT #-} a ~ BFS => SearchStrategy a
instance SearchStrategy BFS
instance SearchStrategy DFS

{-# INLINE query #-}
query
  :: forall tag s tgt out
   . SearchStrategy s
  => Query s tag tgt out
  -> tgt
  -> s out
query (Query f) tgt = f $ Tagged tgt
