{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}

import           Control.Egison
import           Data.List
import           System.Environment
import           Data.Maybe


comb2 :: Int -> [(Int, Int)]
comb2 n = matchAll dfs
                   [1 .. n]
                   (List Something)
                   [[mc| _ ++ $x : _ ++ $y : _ -> (x, y) |]]

comb2Native :: Int -> [(Int, Int)]
comb2Native n = [ (x, y) | (x : ts) <- tails [1 .. n], y <- ts ]

comb2Backtracking :: Int -> [(Int, Int)]
comb2Backtracking n =
  toList
    $   dfs [1 .. n]
    >>= fromList
    .   tails
    >>= (fromList . maybeToList . uncons)
    >>= \(x, ys) ->
          fromList (tails ys)
            >>= (\ys' -> fromList (maybeToList (uncons ys)))
            >>= \(y, _) -> pure (x, y)

comb2Backtracking' :: Int -> [(Int, Int)]
comb2Backtracking' n =
  toList $ dfs (List Something, [1 .. n]) >>= \(mat_a5g3, tgt_a5g4) ->
    let (m_a5g5, m_a5g6) = joinM mat_a5g3 tgt_a5g4
    in
      (   fromList (join (WC, GP) mat_a5g3 tgt_a5g4)
      >>= (\(t_a5g7, t_a5g8) ->
            let (m_a5g9, m_a5ga) = consM m_a5g6 t_a5g8
            in
              (   fromList (cons (GP, GP) m_a5g6 t_a5g8)
              >>= (\(t_a5gb, t_a5gc) ->
                    let x = t_a5gb
                    in
                      let (m_a5gd, m_a5ge) = joinM m_a5ga t_a5gc
                      in
                        (   fromList (join (WC, GP) m_a5ga t_a5gc)
                        >>= (\(t_a5gf, t_a5gg) ->
                              let (m_a5gh, m_a5gi) = consM m_a5ge t_a5gg
                              in
                                (   fromList (cons (GP, WC) m_a5ge t_a5gg)
                                >>= (\(t_a5gj, t_a5gk) ->
                                      let y = t_a5gj in pure (x, y)
                                    )
                                )
                            )
                        )
                  )
              )
          )
      )

comb2' n = matchAll
  dfs
  [1 .. n]
  (List Something)
  [ \(mat_a5g3, tgt_a5g4) ->
      let (m_a5g5, m_a5g6) = joinM mat_a5g3 tgt_a5g4
      in
        (   fromList (join (WC, GP) mat_a5g3 tgt_a5g4)
        >>= (\(t_a5g7, t_a5g8) ->
              let (m_a5g9, m_a5ga) = consM m_a5g6 t_a5g8
              in
                (   fromList (cons (GP, GP) m_a5g6 t_a5g8)
                >>= (\(t_a5gb, t_a5gc) ->
                      let x = t_a5gb
                      in
                        let (m_a5gd, m_a5ge) = joinM m_a5ga t_a5gc
                        in
                          (   fromList (join (WC, GP) m_a5ga t_a5gc)
                          >>= (\(t_a5gf, t_a5gg) ->
                                let (m_a5gh, m_a5gi) =
                                        consM m_a5ge t_a5gg
                                in
                                  (   fromList (cons (GP, WC) m_a5ge t_a5gg)
                                  >>= (\(t_a5gj, t_a5gk) ->
                                        let y = t_a5gj in pure (x, y)
                                      )
                                  )
                              )
                          )
                    )
                )
            )
        )
  ]

comb2'' n = matchAllSingle
  dfs
  [1 .. n]
  (List Something)
  (\(mat_a5g3, tgt_a5g4) ->
    let (m_a5g5, m_a5g6) = joinM mat_a5g3 tgt_a5g4
    in
      (   fromList (join (WC, GP) mat_a5g3 tgt_a5g4)
      >>= (\(t_a5g7, t_a5g8) ->
            let (m_a5g9, m_a5ga) = consM m_a5g6 t_a5g8
            in
              (   fromList (cons (GP, GP) m_a5g6 t_a5g8)
              >>= (\(t_a5gb, t_a5gc) ->
                    let x = t_a5gb
                    in
                      let (m_a5gd, m_a5ge) = joinM m_a5ga t_a5gc
                      in
                        (   fromList (join (WC, GP) m_a5ga t_a5gc)
                        >>= (\(t_a5gf, t_a5gg) ->
                              let (m_a5gh, m_a5gi) = consM m_a5ge t_a5gg
                              in
                                (   fromList (cons (GP, WC) m_a5ge t_a5gg)
                                >>= (\(t_a5gj, t_a5gk) ->
                                      let y = t_a5gj in pure (x, y)
                                    )
                                )
                            )
                        )
                  )
              )
          )
      )
  )

main = do
  [fn, n] <- getArgs
  let fn' = read fn :: Int
  let n'  = read n :: Int
  case fn' of
    -- n'=5000, 0.218s
    1 -> print $ length $ comb2 n'
    -- n'=5000, 0.077s
    2 -> print $ length $ comb2Native n'
    -- n'=5000, 0.083s
    3 -> print $ length $ comb2Backtracking n'
    -- n'=5000, 0.136s
    4 -> print $ length $ comb2Backtracking' n'
    -- n'=5000, 
    5 -> print $ length $ comb2' n'
    -- n'=5000, 
    6 -> print $ length $ comb2'' n'
