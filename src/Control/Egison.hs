-- |
--
-- Module:      Control.Egsion
-- Description: Battery Included Re-exports
-- Stability:   experimental
--
-- This module provides a "Battery Included" set of re-exports

module Control.Egison
  ( module X
  , mc
  )
where

-- re-exports
import           Control.Egison.Match          as X
import           Control.Egison.Matcher        as X

import           Control.Monad.Search          as X

import           Data.Query                    as X
import           Data.Query.QQ                 as X
import           Data.Query.Pattern.Collection as X
import           Data.Query.Pattern.Tuple      as X
import           Data.Query.Pattern.Value      as X

-- Matchers
import           Control.Egison.Matcher.List   as X
import           Control.Egison.Matcher.Multiset
                                               as X
import           Control.Egison.Matcher.Set    as X
import           Control.Egison.Matcher.Pair   as X
import           Control.Egison.Matcher.UnorderedPair
                                               as X


-- local imports
import           Language.Haskell.TH.Quote      ( QuasiQuoter )

mc :: QuasiQuoter
mc = q
