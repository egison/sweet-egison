-- |
--
-- Module:      Data.Query
-- Description: Types and combinators for data queries
-- Stability:   experimental
--
-- This module defines types and combinators for data queries, the intuitive way to query against various kind of data using pattern matching.

module Data.Query
  ( Query(..)
  , SearchStrategy
  , query
  , query'
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Control.Monad                  ( mzero )
import           Control.Applicative            ( Alternative(..) )
import           Control.Monad.Search           ( MonadSearch
                                                , BFS
                                                , DFS
                                                )


-- | Type of queries.
--
-- We can construct 'Query' values using the quasi-quoter provided in 'Data.Query.QQ'.
newtype Query strategy tag tgt out = Query { unQuery :: Proxy tag -> tgt -> strategy out }

instance MonadSearch m => Semigroup (Query m tag tgt out) where
  {-# INLINE (<>) #-}
  Query a <> Query b = Query $ \p x -> a p x <|> b p x

instance MonadSearch m => Monoid (Query m tag tgt out) where
  {-# INLINABLE mempty #-}
  mempty = Query $ \_ _ -> mzero


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
-- | @'query' \@tag q tgt@ performs a search specifies by @q@ against @tgt@ tagged with @tag@.
--
-- You can optionally specify searching strategy like @'query' @tag @'DFS'@. This searching strategy is defaulted to 'BFS' if not specified.
query
  :: forall tag s tgt out
   . SearchStrategy s
  => Query s tag tgt out
  -> tgt
  -> s out
query = query'

{-# INLINE query' #-}
-- | 'query'' is equivalent to 'query', expect this does not default searching strategy to 'BFS'.
query'
  :: forall tag s tgt out
   . MonadSearch s
  => Query s tag tgt out
  -> tgt
  -> s out
query' (Query f) = f Proxy
