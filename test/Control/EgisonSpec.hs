module Control.EgisonSpec
  (
    test_something
  , test_list
  , test_multiset
  , test_set
  )
where

import           Control.Egison

import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Numbers.Primes            ( primes )

pmap :: (a -> b) -> [a] -> [b]
pmap f xs = matchAll dfs xs (List Something) [[mc| _ ++ $x : _ -> f x |]]

pmember :: Eq a => a -> [a] -> Bool
pmember x xs =
  match dfs xs (Multiset Eql) [[mc| #x : _ -> True |], [mc| _ -> False |]]

test_something :: [TestTree]
test_something =
  [
   testCase "Something and wildcard"
    $ assertEqual "simple" [1]
    $ matchAll dfs 2 Something [[mc| _ -> 1 |]]
  ,testCase "Something and pattern variable"
    $ assertEqual "simple" [2]
    $ matchAll dfs 2 Something [[mc| $x -> x |]]
   ]

test_list :: [TestTree]
test_list =
  [
    testCase "The cons pattern for lists"
    $ assertEqual "simple" [(1, [2, 3])]
    $ matchAll dfs [1, 2, 3] (List Something) [[mc| cons $x $xs -> (x, xs) |]]
  , testCase "The infix cons pattern for lists"
    $ assertEqual "simple" [(1, [2, 3])]
    $ matchAll dfs [1, 2, 3] (List Something) [[mc| $x : $xs -> (x, xs) |]]
  , testCase "The infix join pattern for lists"
    $ assertEqual "simple" [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
    $ matchAll dfs [1, 2, 3] (List Something) [[mc| $hs ++ $ts -> (hs, ts) |]]
  , testCase "The join-cons pattern"
    $ assertEqual "simple" [([],1,[2,3]),([1],2,[3]),([1,2],3,[])]
    $ matchAll dfs ([1, 2, 3] :: [Integer]) (List Something) [[mc| $hs ++ $x : $ts -> (hs, x, ts) |]]
  , testCase "The join-cons value pattern"
    $ assertEqual "simple" [([1],[3])]
    $ matchAll dfs ([1, 2, 3] :: [Integer]) (List Eql) [[mc| $hs ++ #2 : $ts -> (hs, ts) |]]
  , testCase "cons pattern for list (infinite)"
    $ assertEqual "simple" [1]
    $ matchAll dfs [1 ..] (List Something) [[mc| $x : _ -> x |]]
  , testCase "'map' defined using matchAllDFS"
    $ assertEqual "simple" [2, 4, 6]
    $ take 3
    $ pmap (* 2) [1 ..]
  ]

test_multiset :: [TestTree]
test_multiset =
  [
     testCase "cons pattern for multiset"
      $ assertEqual "simple" [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
      $ matchAll dfs [1, 2, 3] (Multiset Something) [[mc| $x : $xs -> (x, xs) |]]
   , testCase "'member' defined using matchAllDFS"
      $ assertEqual "simple" False
      $ pmember 2 [1, 3, 4]
  ]

test_set :: [TestTree]
test_set =
  [
--     testCase "cons pattern for multiset"
--      $ assertEqual "simple" [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
--      $ matchAllDFS [1, 2, 3] (Set IntegralM) [[mc| $x : $y : _ -> (x, y) |]]
--     testCase "cons pattern for multiset"
--      $ assertEqual "simple" [(1, 1), (1, 2), (2, 1), (1, 3), (2, 2), (3, 1), (2, 3), (3, 2), (3, 3)]
--      $ matchAll [1, 2, 3] (Set IntegralM) [[mc| $x : $y : _ -> (x, y) |]]
  ]
