{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Egison
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  let n = read (head args)
  let ans = matchAll @DFS $ [1 .. n] `with` [search|
    (tails (cons $x (tails (cons $y _)))) => (x, y)
  |]
  putStrLn . show $ length ans
