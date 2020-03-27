module Control.EgisonSpec
  ( test_list
  , test_multiset
  , test_infinite
  , test_not
  , test_predicate
  , test_prime
  )
where

import           Control.Egison

import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Numbers.Primes            ( primes )


pmap :: (a -> b) -> [a] -> [b]
pmap f xs = matchAll @DFS xs (List Something) [q| _ ++ $x : _ -> f x |]

pmember :: Eq a => a -> [a] -> Bool
pmember x xs =
  match @DFS xs (Multiset EqM) $ [q| #x : _ -> True |] <> [q| _ -> False |]

test_list :: [TestTree]
test_list =
  [ testCase "cons pattern for list"
    $ assertEqual "simple" [(1, [2, 3])]
    $ matchAll @BFS
        [1, 2, 3]
        (List IntegralM)
        [q|
        $x : $xs -> (x, xs)
      |]
  , testCase "cons pattern for list (infinite)"
    $ assertEqual "simple" [1]
    $ matchAll @BFS
        [1 ..]
        (List IntegralM)
        [q|
        $x : _ -> x
      |]
  , testCase "join pattern for list"
    $ assertEqual "length" 6
    $ length
    $ matchAll @BFS
        [1 .. 5]
        (List IntegralM)
        [q|
        $xs ++ $ys -> (xs, ys)
      |]
  , testCase "'map' defined using matchAll"
    $ assertEqual "simple" [2, 4, 6]
    $ take 3
    $ pmap (* 2) [1 ..]
  , testCase "'member' defined using matchAll"
    $ assertEqual "simple" False
    $ pmember 2 [1, 3, 4]
  ]

test_multiset :: [TestTree]
test_multiset =
  [ testCase "cons pattern for multiset"
      $ assertEqual "simple" [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
      $ matchAll @BFS
          [1, 2, 3]
          (Multiset IntegralM)
          [q|
          $x : $xs -> (x, xs)
        |]
  ]

test_infinite :: [TestTree]
test_infinite =
  [ testCase "multiset bfs order"
    $ assertEqual
        "simple"
        [ (1, 2)
        , (2, 1)
        , (1, 3)
        , (3, 1)
        , (1, 4)
        , (2, 3)
        , (1, 5)
        , (4, 1)
        , (1, 6)
        , (2, 4)
        ]
    $ take 10
    $ matchAll @BFS
        [1 ..]
        (Multiset IntegralM)
        [q|
        $x : $y : _ -> (x, y)
      |]
  , testCase "set bfs order"
    $ assertEqual
        "simple"
        [ (1, 1)
        , (2, 1)
        , (1, 2)
        , (3, 1)
        , (1, 3)
        , (2, 2)
        , (1, 4)
        , (4, 1)
        , (1, 5)
        , (2, 3)
        ]
    $ take 10
    $ matchAll @BFS
        [1 ..]
        (Set IntegralM)
        [q|
        $x : $y : _ -> (x, y)
      |]
  , testCase "use bfs if omitted"
    $ assertEqual
        "simple"
        [ (1, 1)
        , (2, 1)
        , (1, 2)
        , (3, 1)
        , (1, 3)
        , (2, 2)
        , (1, 4)
        , (4, 1)
        , (1, 5)
        , (2, 3)
        ]
    $ take 10
    $ matchAll
        [1 ..]
        (Set IntegralM)
        [q|
        $x : $y : _ -> (x, y)
      |]
  , testCase "set dfs order"
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
    $ matchAll @DFS
        [1 ..]
        (Set IntegralM)
        [q|
        $x : $y : _ -> (x, y)
      |]
  ]

test_predicate :: [TestTree]
test_predicate =
  [ testCase "predicate pattern"
      $ assertEqual "simple" [2, 4, 6, 8, 10]
      $ matchAll @BFS
          [1 .. 10]
          (Multiset IntegralM)
          [q|
          (?(\x -> mod x 2 == 0) & $x) : _ -> x
        |]
  ]

test_not :: [TestTree]
test_not =
  [ testCase "not pattern" $ assertEqual "simple" [1, 3, 2] $ matchAll @BFS
      [1, 1, 2, 3, 1, 3, 2]
      (List IntegralM)
      [q|
          _ ++ $x : !(_ ++ #x : _) -> x
        |]
  ]

test_prime :: [TestTree]
test_prime =
  [ testCase "prime twins"
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
    $ matchAll @BFS
        primes
        (List IntegralM)
        [q|
        _ ++ $p : #(p+2) : _ -> (p, p+2)
      |]
  , testCase "(p, p+6)"
    $ assertEqual "simple" [(5, 11), (7, 13), (11, 17), (13, 19), (17, 23)]
    $ take 5
    $ matchAll @BFS
        primes
        (List IntegralM)
        [q|
        _ ++ $p : _ ++ #(p+6) : _ -> (p, p+6)
      |]
  , testCase "prime triplets"
    $ assertEqual
        "simple"
        [ (5  , 7  , 11)
        , (11 , 13 , 17)
        , (7  , 11 , 13)
        , (17 , 19 , 23)
        , (13 , 17 , 19)
        , (41 , 43 , 47)
        , (37 , 41 , 43)
        , (67 , 71 , 73)
        , (101, 103, 107)
        , (97 , 101, 103)
        ]
    $ take 10
    $ matchAll @BFS
        primes
        (List IntegralM)
        [q|
        _ ++ $p : ($m & (#(p+2) | #(p+4))) : #(p+6) : _ -> (p, m, p+6)
      |]
  ]
