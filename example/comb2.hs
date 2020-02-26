{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args)
  let ans = matchAll @DFS $ [1 .. n] `with` [search|
    _ ++ $x : (_ ++ $y : _) => (x, y)
  |]
  print $ length ans
