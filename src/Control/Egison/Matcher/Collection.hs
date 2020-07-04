-- |
--
-- Module:      Control.Egison.Matcher.Collection
-- Description: Matchers for collection data types
-- Stability:   experimental

module Control.Egison.Matcher.Collection
  ( CollectionPattern(..)
  , List(..)
  , Multiset(..)
  , Set(..)
  )
where

import           Data.List                      ( tails )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Monad.Search
import           Control.Egison.Match
import           Control.Egison.Matcher
import           Control.Egison.Matcher.Pair
import           Control.Egison.QQ
import           Language.Egison.Syntax.Pattern
                                               as Pat
                                                ( Expr(..) )
import           Language.Haskell.TH            ( Exp(..)
                                                , Name
                                                )

-- | Class for collection pattern constructors.
class CollectionPattern m t where
  -- | Matcher for the elements.
  type ElemM m
  -- | Type of the target elements.
  type ElemT t
  -- | Pattern that matches with empty collections.
  -- @[]@ is desugared into 'nil' by the quasi-quoter.
  nil :: Pattern () m t ()
  nilM :: m -> t -> ()
  default nilM :: m -> t -> ()
  {-# INLINE nilM #-}
  nilM _ _ = ()
  -- | Pattern that destructs collections into its head and tail.
  -- @:@ is desugared into 'cons' by the quasi-quoter.
  cons :: Pattern (PP, PP) m t (ElemT t, t)
  consM :: m -> t -> (ElemM m, m)
  -- | Pattern that destructs collections into its initial prefix and remaining suffix.
  -- @++@ is desugared into 'join' by the quasi-quoter.
  join :: Pattern (PP, PP) m t (t, t)
  joinM :: m -> t -> (m, m)
  default joinM :: m -> t -> (m, m)
  {-# INLINE joinM #-}
  joinM m _ = (m, m)
  elm :: Pattern PP m t (ElemT t)
  elmM :: m -> t -> ElemM m

-- | 'List' matcher is a matcher for collections that matches as if they're normal lists.
newtype List m = List m

instance Matcher m t => Matcher (List m) [t]

instance Matcher m t => CollectionPattern (List m) [t] where
  type ElemM (List m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ _ [] = pure ()
  nil _ _ _  = mzero
  {-# INLINE cons #-}
  cons _ _        []       = mzero
  cons _ (List m) (x : xs) = pure (x, xs)
  {-# INLINE consM #-}
  consM (List m) _ = (m, List m)
  {-# INLINABLE join #-}
  join (WC, _) m xs       = map (\ts -> (undefined, ts)) (tails xs)
  join _       m []       = pure ([], [])
  join ps      m (x : xs) = pure ([], x : xs) `mplus` do
    (ys, zs) <- join ps m xs
    pure (x : ys, zs)
  {-# INLINE elm #-}
  elm _ (List m) xs = xs
  {-# INLINE elmM #-}
  elmM (List m) _ = m

instance (Eq a, Matcher m a, ValuePattern m a) => ValuePattern (List m) [a] where
  value e () (List m) v = if eqAs (List m) (List m) e v then pure () else mzero

newtype Multiset m = Multiset m

instance Matcher m t => Matcher (Multiset m) [t]

instance Matcher m t => CollectionPattern (Multiset m) [t] where
  type ElemM (Multiset m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ _ [] = pure ()
  nil _ _ _  = mzero
  {-# INLINE cons #-}
  cons (_, WC) (Multiset m) xs = map (\x -> (x, undefined)) xs
  cons _       (Multiset m) xs = matchAll
    dfs
    xs
    (List Something)
    [[mc| $hs ++ $x : $ts -> (x, hs ++ ts) |]]
  {-# INLINE consM #-}
  consM (Multiset m) _ = (m, Multiset m)
  {-# INLINABLE join #-}
  join = undefined

instance (Eq a, Matcher m a, ValuePattern m a) => ValuePattern (Multiset m) [a] where
  value e () (Multiset m) v =
    if eqAs (List m) (Multiset m) e v then pure () else mzero

newtype Set m = Set m

instance Matcher m t => Matcher (Set m) [t]

instance Matcher m t => CollectionPattern (Set m) [t] where
  type ElemM (Set m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ _ [] = pure ()
  nil _ _ _  = mzero
  {-# INLINE cons #-}
  cons (_, WC) (Set m) xs = map (\x -> (x, undefined)) xs
  cons _       (Set m) xs = map (\x -> (x, xs)) xs
  {-# INLINE consM #-}
  consM (Set m) _ = (m, Set m)
  {-# INLINABLE join #-}
  join = undefined

instance (Eq a, Matcher m a, ValuePattern m a) => ValuePattern (Set m) [a] where
  value e () (Set m) v = if eqAs (List m) (Set m) e v then pure () else mzero

eqAs m1 m2 xs ys = match
  dfs
  (xs, ys)
  (Pair m1 m2)
  [ [mc| ([], []) -> True |]
  , [mc| ($x : $xs, #x : $ys) -> eqAs m1 m2 xs ys |]
  , [mc| _ -> False |]
  ]
