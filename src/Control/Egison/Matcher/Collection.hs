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
  cons :: Pattern (PP (ElemT t), PP t) m t (ElemT t, t)
  consM :: m -> t -> (ElemM m, m)
  -- | Pattern that destructs collections into its initial prefix and remaining suffix.
  -- @++@ is desugared into 'join' by the quasi-quoter.
  join :: Pattern (PP t, PP t) m t (t, t)
  joinM :: m -> t -> (m, m)
  default joinM :: m -> t -> (m, m)
  {-# INLINE joinM #-}
  joinM m _ = (m, m)
  elm :: Pattern (PP (ElemT t)) m t (ElemT t)
  elmM :: m -> t -> ElemM m
  joinCons :: Pattern (PP t, PP (ElemT t), PP t) m t (t, ElemT t, t)
  joinConsM :: m -> t -> (m, ElemM m, m)
  app :: Pattern (PP t, PP t) m t (t -> t, t)
  appM :: m -> t -> (Something, m)
  default appM :: m -> t -> (Something, m)
  {-# INLINE appM #-}
  appM m _ = (Something, m)
  appCons :: Pattern (PP (t -> t), PP (ElemT t), PP t) m t (t -> t, ElemT t, t)
  appConsM :: m -> t -> (Something, ElemM m, m)

-- | 'List' matcher is a matcher for collections that matches as if they're normal lists.
newtype List m = List m

instance Matcher m t => Matcher (List m) [t]

instance Matcher m t => CollectionPattern (List m) [t] where
  {-# SPECIALIZE instance Matcher m t => CollectionPattern (List m) [t] #-}
  type ElemM (List m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ _ [] = pure ()
  nil _ _ _  = mzero
  {-# INLINE cons #-}
  cons _ _        []       = mzero
  cons _ (List _) (x : xs) = pure (x, xs)
  {-# INLINE consM #-}
  consM (List m) _ = (m, List m)
  {-# INLINABLE join #-}
  join (WC, _) _ xs       = map (\ts -> (undefined, ts)) (tails xs)
  join _       _ []       = pure ([], [])
  join ps      m (x : xs) = pure ([], x : xs) `mplus` do
    (ys, zs) <- join ps m xs
    pure (x : ys, zs)
  {-# INLINE elm #-}
  elm _ (List _) xs = xs
  {-# INLINE elmM #-}
  elmM (List m) _ = m
  {-# INLINE joinCons #-}
  joinCons (WC, _, WC) (List _) tgt = [ (undefined, x, undefined) | x <- tgt ]
  joinCons (WC, _, _) (List _) tgt  = [ (undefined, x, rs) | (x:rs) <- tails tgt ]
  joinCons (_, _, _) (List _) tgt   = f [] tgt
   where
    f _ [] = []
    f rhs (x : ts) = (reverse rhs, x, ts) : f (x : rhs) ts
  {-# INLINE joinConsM #-}
  joinConsM (List m) _ = (List m, m, List m)
  {-# INLINABLE app #-}
  app (WC, _) _ xs       = map (\ts -> (undefined, ts)) (tails xs)
  app _       _ []       = pure (id, [])
  app ps      m (x : xs) = pure (id, x : xs) `mplus` do
    (ys, zs) <- app ps m xs
    pure (\ls -> x : ys ls, zs)
  {-# INLINABLE appCons #-}
  appCons (WC, _, WC) (List _) tgt = [ (undefined, x, undefined) | x <- tgt ]
  appCons (WC, _, _) (List _) tgt  = [ (undefined, x, rs) | (x:rs) <- tails tgt ]
  appCons (_, _, _) (List _) tgt   = f id tgt
   where
    f _ [] = []
    f hs (x : ts) = (hs, x, ts) : f (hs . (x :)) ts
  {-# INLINABLE appConsM #-}
  appConsM (List m) _ = (Something, m, List m)

instance (Eq a, Matcher m a, ValuePattern m a) => ValuePattern (List m) [a] where
  value e () (List m) v = if eqAs (List m) (List m) e v then pure () else mzero

newtype Multiset m = Multiset m

instance Matcher m t => Matcher (Multiset m) [t]

instance Matcher m t => CollectionPattern (Multiset m) [t] where
  {-# SPECIALIZE instance Matcher m t => CollectionPattern (Multiset m) [t] #-}
  type ElemM (Multiset m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ _ [] = pure ()
  nil _ _ _  = mzero
  {-# INLINE cons #-}
  cons (_, WC) (Multiset _) xs = map (\x -> (x, undefined)) xs
  cons _       (Multiset _) xs = matchAll
    dfs
    xs
    (List Something)
    [[mc| $hs ++ $x : $ts -> (x, hs ++ ts) |]]
  {-# INLINE consM #-}
  consM (Multiset m) _ = (m, Multiset m)
  {-# INLINABLE join #-}
  join = undefined
  elm = undefined
  elmM = undefined
  joinCons = undefined
  joinConsM = undefined
  app = undefined
  appCons = undefined
  appConsM = undefined

instance (Eq a, Matcher m a, ValuePattern m a) => ValuePattern (Multiset m) [a] where
  value e () (Multiset m) v =
    if eqAs (List m) (Multiset m) e v then pure () else mzero

newtype Set m = Set m

instance Matcher m t => Matcher (Set m) [t]

instance Matcher m t => CollectionPattern (Set m) [t] where
  {-# SPECIALIZE instance Matcher m t => CollectionPattern (Set m) [t] #-}
  type ElemM (Set m) = m
  type ElemT [t] = t
  {-# INLINE nil #-}
  nil _ _ [] = pure ()
  nil _ _ _  = mzero
  {-# INLINE cons #-}
  cons (_, WC) (Set _) xs = map (\x -> (x, undefined)) xs
  cons _       (Set _) xs = map (\x -> (x, xs)) xs
  {-# INLINE consM #-}
  consM (Set m) _ = (m, Set m)
  {-# INLINABLE join #-}
  join = undefined
  elm = undefined
  elmM = undefined
  joinCons = undefined
  joinConsM = undefined
  app = undefined
  appCons = undefined
  appConsM = undefined

instance (Eq a, Matcher m a, ValuePattern m a) => ValuePattern (Set m) [a] where
  value e () (Set m) v = if eqAs (List m) (Set m) e v then pure () else mzero

eqAs :: (Matcher m1 t1, Matcher m2 t2,
         ValuePattern (ElemM m2) (ElemT t2), ElemT t1 ~ ElemT t2,
         CollectionPattern m1 t1, CollectionPattern m2 t2) =>
        m1 -> m2 -> t1 -> t2 -> Bool
eqAs m1 m2 xs ys = match
  dfs
  (xs, ys)
  (m1, m2)
  [ [mc| ([], []) -> True |]
  , [mc| ($x : $xs, #x : $ys) -> eqAs m1 m2 xs ys |]
  , [mc| _ -> False |]
  ]
