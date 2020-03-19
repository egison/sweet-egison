{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}

module Control.Egison.Match
  ( matchAll
  , match
  )
where

import           Control.Egison.Matcher         ( Matcher(..) )
import           Control.Monad.Search           ( MonadSearch(..) )
import           Data.Query                     ( Query
                                                , find
                                                )


{-# INLINABLE match #-}
match
  :: forall strategy matcher out
   . (Matcher matcher, MonadSearch strategy)
  => Target matcher
  -> forall m . m ~ matcher => Query strategy matcher out -> out
match tgt = head . matchAll tgt

{-# INLINABLE matchAll #-}
matchAll
  :: forall strategy matcher out
   . (Matcher matcher, MonadSearch strategy)
  => Target matcher
  -> forall m . m ~ matcher => Query strategy matcher out -> [out]
matchAll tgt q = collect . find q $ wrap tgt
