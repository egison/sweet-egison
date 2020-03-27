{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Control.Egison.Match
  ( matchAll
  , match
  , SearchStrategy
  )
where

import           Control.Egison.Matcher         ( Matcher )
import           Control.Monad.Search           ( MonadSearch(..)
                                                , BFS
                                                , DFS
                                                )
import           Data.Query                     ( Query
                                                , find
                                                )

-- | Class that represents search strategies used in pattern matching.
-- In the view of implementors, this is a helper class to default search strategy to BFS in 'match' and 'matchAll'.
--
-- When the strategy is explicitly specified like @'match' \@BFS@, this class appears as @'SearchStrategy' 'BFS'@ in the context. This constraint matches to @'SearchStrategy' a@ and @'SearchStrategy' 'BFS'@ instances. The latter is more specific so would be chosen. @'SearchStrategy' 'DFS'@ also typechecks in the similar way.
--
-- When the strategy is not specified, the parameter is left uninstantiated as @'SearchStrategy' a0@. This constraint only matches to @'SearchStrategy' a@ instance and GHC will choose it since it is @INCOHERENT@. Once the instance is chosen, the constraint @a ~ 'BFS'@ is introduced and @a@ is defaulted to 'BFS'. @INCOHERENT@ is necessary for GHC to cut short in the decision. Without @INCOHERENT@, GHC looks for further instances that /unify/ with @SearchStrategy a0@ but not /match/ it and fails because of them.
--
-- For details, see [the relevant section in GHC User's Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#overlapping-instances).
class MonadSearch m => SearchStrategy m
instance {-# INCOHERENT #-} a ~ BFS => SearchStrategy a
instance SearchStrategy BFS
instance SearchStrategy DFS

{-# INLINABLE match #-}
match
  :: forall strategy matcher target out
   . (Matcher matcher target, SearchStrategy strategy)
  => target
  -> forall m . m ~ matcher => Query strategy matcher target out -> out
match tgt = head . matchAll tgt

{-# INLINABLE matchAll #-}
matchAll
  :: forall strategy matcher target out
   . (Matcher matcher target, SearchStrategy strategy)
  => target
  -> forall m . m ~ matcher => Query strategy matcher target out -> [out]
matchAll tgt q = collect $ find q tgt
