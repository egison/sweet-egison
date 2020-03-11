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
import           Control.Egison.Matcher.Multiset
import           Control.Egison.Matcher.List
import           Control.Egison.Matcher.Set

import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.Numbers.Primes            ( primes )


test_list :: [TestTree]
test_list =
  [ testCase "cons pattern for list" $ do
    assertEqual "simple" [(1, [2, 3])] $ matchAll @BFS @(List (M Int))
      [1, 2, 3]
      [query|
          $x : $xs -> (x, xs)
        |]
  , testCase "cons pattern for list (infinite)" $ do
    assertEqual "simple" [1] $ matchAll @BFS @(List (M Int))
      [1 ..]
      [query|
          $x : _ -> x
        |]
  , testCase "join pattern for list" $ do
    assertEqual "length" 6 $ length $ matchAll @BFS @(List (M Int))
      [1 .. 5]
      [query|
          $xs ++ $ys -> (xs, ys)
        |]
  ]

test_multiset :: [TestTree]
test_multiset =
  [ testCase "cons pattern for multiset" $ do
      assertEqual "simple" [(1, [2, 3]), (2, [1, 3]), (3, [1, 2])]
        $ matchAll @BFS @(Multiset (M Int))
            [1, 2, 3]
            [query|
          $x : $xs -> (x, xs)
        |]
  ]

test_infinite :: [TestTree]
test_infinite =
  [ testCase "multiset bfs order" $ do
    assertEqual
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
      $ matchAll @BFS @(Multiset (M Int))
          [1 ..]
          [query|
        $x : $y : _ -> (x, y)
      |]
  , testCase "set bfs order" $ do
    assertEqual
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
      $ matchAll @BFS @(Set (M Int))
          [1 ..]
          [query|
        $x : $y : _ -> (x, y)
      |]
  , testCase "set dfs order" $ do
    assertEqual
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
      $ matchAll @DFS @(Set (M Int))
          [1 ..]
          [query|
        $x : $y : _ -> (x, y)
      |]
  ]

test_predicate :: [TestTree]
test_predicate =
  [ testCase "predicate pattern" $ do
      assertEqual "simple" [2, 4, 6, 8, 10] $ matchAll @BFS @(Multiset (M Int))
        [1 .. 10]
        [query|
      (?(\x -> mod x 2 == 0) & $x) : _ -> x
    |]
  ]

test_not :: [TestTree]
test_not =
  [ testCase "not pattern" $ do
      assertEqual "simple" [1, 3, 2] $ matchAll @BFS @(List (M Int))
        [1, 1, 2, 3, 1, 3, 2]
        [query| _ ++ $x : !(_ ++ #x : _) -> x |]
  ]

test_prime :: [TestTree]
test_prime =
  [ testCase "prime twins" $ do
    assertEqual
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
      $ matchAll @BFS @(List (M Int))
          primes
          [query|
      _ ++ $p : #(p+2) : _ -> (p, p+2)
    |]
  , testCase "(p, p+6)" $ do
    assertEqual "simple" [(5, 11), (7, 13), (11, 17), (13, 19), (17, 23)]
      $ take 5
      $ matchAll @BFS @(List (M Int))
          primes
          [query|
        _ ++ $p : _ ++ #(p+6) : _ -> (p, p+6)
      |]
  , testCase "prime triplets" $ do
    assertEqual
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
      $ matchAll @BFS @(List (M Int))
          primes
          [query|
        _ ++ $p : ($m & (#(p+2) | #(p+4))) : #(p+6) : _ -> (p, m, p+6)
      |]
  ]
