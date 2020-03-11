{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           Control.Egison.Matcher.List
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args)
  let ans = matchAll @DFS @(List (M Int)) [1 .. n] [q|
    _ ++ $x : _ ++ $y : _ -> (x, y)
  |]
  print $ length ans
