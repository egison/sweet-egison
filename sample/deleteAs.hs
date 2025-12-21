{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Egison

deleteWith m x xs =
  match dfs xs (List m) [[mc| $hs ++ #x : $ts -> hs ++ ts |], [mc| _ -> xs |]]

main = do
  --  print $ deleteWith (List Eql) [1, 2] [[2, 3], [2, 1], [2, 4]] -- [[2, 3], [2, 1], [2, 4]]
  --  print $ deleteWith (Multiset Eql) [1, 2] [[2, 3], [2, 1], [2, 4]] -- [[2, 3], [2, 4]]
  print $
    deleteWith
      (List Eql)
      ([1, 2] :: [Int])
      ([[2, 3], [2, 1], [2, 4]] :: [[Int]]) -- [[2, 3], [2, 1], [2, 4]]
  print $
    deleteWith
      (Multiset Eql)
      ([1, 2] :: [Int])
      ([[2, 3], [2, 1], [2, 4]] :: [[Int]]) -- [[2, 3], [2, 4]]
