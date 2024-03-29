module Control.EgisonSpec
  ( test_something
  , test_pair
  , test_list
  , test_multiset
  , test_set
  , test_prime
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

punique :: Eq a => [a] -> [a]
punique xs = matchAll dfs xs (List Eql) [[mc| _ ++ $x : !(_ ++ #x : _) -> x |]]


test_something :: [TestTree]
test_something =
  [ testCase "Something and wildcard" $ assertEqual "simple" [1] $ matchAll
    dfs
    2
    Something
    [[mc| _ -> 1 |]]
  , testCase "Something and pattern variable"
    $ assertEqual "simple" [2]
    $ matchAll dfs 2 Something [[mc| $x -> x |]]
  ]

test_pair :: [TestTree]
test_pair =
  [ testCase "pair" $ assertEqual "simple" (1, 2) $ match
      dfs
      (1, 2)
      (Something, Something)
      [[mc| ($x, $y) -> (x, y) |]]
  ]

test_list :: [TestTree]
test_list =
  [ testCase "The cons pattern for lists"
    $ assertEqual "simple" [(1, [2, 3])]
    $ matchAll dfs [1, 2, 3] (List Something) [[mc| cons $x $xs -> (x, xs) |]]
  , testCase "The infix cons pattern for lists"
    $ assertEqual "simple" [(1, [2, 3])]
    $ matchAll dfs [1, 2, 3] (List Something) [[mc| $x : $xs -> (x, xs) |]]
  , testCase "The infix join pattern for lists"
    $ assertEqual
        "simple"
        [([], [1, 2, 3]), ([1], [2, 3]), ([1, 2], [3]), ([1, 2, 3], [])]
    $ matchAll dfs [1, 2, 3] (List Something) [[mc| $hs ++ $ts -> (hs, ts) |]]
  , testCase "The join-cons pattern"
    $ assertEqual "simple" [([], 1, [2, 3]), ([1], 2, [3]), ([1, 2], 3, [])]
    $ matchAll dfs
               ([1, 2, 3] :: [Integer])
               (List Something)
               [[mc| $hs ++ $x : $ts -> (hs, x, ts) |]]
  , testCase "The join-cons value pattern"
    $ assertEqual "simple" [([1], [3])]
    $ matchAll dfs
               ([1, 2, 3] :: [Integer])
               (List Eql)
               [[mc| $hs ++ #2 : $ts -> (hs, ts) |]]
  , testCase "cons pattern for list (infinite)"
    $ assertEqual "simple" [1]
    $ matchAll dfs [1 ..] (List Something) [[mc| $x : _ -> x |]]
  , testCase "'map' defined using matchAllDFS"
    $ assertEqual "simple" [2, 4, 6]
    $ take 3
    $ pmap (* 2) [1 ..]
  , testCase "'unique' defined using matchAllDFS"
    $ assertEqual "simple" [1, 3, 5, 2, 4]
    $ punique [1, 2, 3, 4, 5, 2, 4]
  ]

test_multiset :: [TestTree]
test_multiset =
  [ testCase "cons pattern for multiset"
    $ assertEqual "simple" [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
    $ matchAll dfs [1, 2, 3] (Multiset Something) [[mc| $x : $xs -> (x, xs) |]]
  , testCase "'member' defined using matchAllDFS"
    $ assertEqual "simple" False
    $ pmember 2 [1, 3, 4]
  ]

test_set :: [TestTree]
test_set =
  [ testCase "double cons pattern for set (dfs)"
    $ assertEqual
        "simple"
        [ (1, 1)
        , (1, 2)
        , (1, 3)
        , (1, 4)
        , (1, 5)
        , (1, 6)
        , (1, 7)
        , (1, 8)
        , (1, 9)
        , (1, 10)
        ]
    $ take 10
    $ matchAll dfs [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
  , testCase "double cons pattern for set (bfs)"
    $ assertEqual
        "simple"
        [ (1, 1)
        , (1, 2)
        , (2, 1)
        , (1, 3)
        , (2, 2)
        , (3, 1)
        , (1, 4)
        , (2, 3)
        , (3, 2)
        , (4, 1)
        ]
    $ take 10
    $ matchAll bfs [1 ..] (Set Something) [[mc| $x : $y : _ -> (x, y) |]]
  ]

test_prime :: [TestTree]
test_prime =
  [ testCase "prime twins (value pattern)"
    $ assertEqual
        "simple"
        [ (3  , 5)
        , (5  , 7)
        , (11 , 13)
        , (17 , 19)
        , (29 , 31)
        , (41 , 43)
        , (59 , 61)
        , (71 , 73)
        , (101, 103)
        , (107, 109)
        ]
    $ take 10
    $ matchAll bfs primes (List Eql) [[mc| _ ++ $p : #(p+2) : _ -> (p, p+2) |]]
  , testCase "prime twins (predicate pattern)"
    $ assertEqual
        "simple"
        [ (3  , 5)
        , (5  , 7)
        , (11 , 13)
        , (17 , 19)
        , (29 , 31)
        , (41 , 43)
        , (59 , 61)
        , (71 , 73)
        , (101, 103)
        , (107, 109)
        ]
    $ take 10
    $ matchAll bfs
               primes
               (List Eql)
               [[mc| _ ++ $p : ?(\x -> x == p + 2) : _ -> (p, p+2) |]]
  , testCase "(p, p+6)"
    $ assertEqual "simple" [(5, 11), (7, 13), (11, 17), (13, 19), (17, 23)]
    $ take 5
    $ matchAll bfs
               primes
               (List Eql)
               [[mc| _ ++ $p : _ ++ #(p+6) : _ -> (p, p+6) |]]
  , testCase "prime triplets"
    $ assertEqual
        "simple"
        [ (5  , 7  , 11)
        , (7  , 11 , 13)
        , (11 , 13 , 17)
        , (13 , 17 , 19)
        , (17 , 19 , 23)
        , (37 , 41 , 43)
        , (41 , 43 , 47)
        , (67 , 71 , 73)
        , (97 , 101, 103)
        , (101, 103, 107)
        ]
    $ take 10
    $ matchAll bfs
               primes
               (List Eql)
               [[mc| _ ++ $p : $m : #(p+6) : _ -> (p, m, p+6) |]]
  ]
