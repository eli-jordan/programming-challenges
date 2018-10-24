{-# LANGUAGE LambdaCase #-}
module Challenge9 where

import Data.List

luckyNumbers :: [Int]
luckyNumbers = sieve 3 [1, 3..]
  where
    sieve i (ln : s : xs) =
      ln : sieve (i + 1) (s : [x | (n, x) <- zip [i..] xs, rem n s /= 0])

luckyNeighbours :: Int -> Maybe (Int, Int)
luckyNeighbours n =
  let
    luckyContext = zip luckyNumbers (drop 1 luckyNumbers)
  in
    find (\case (_, big) -> big >= n) luckyContext

runForInput :: Int -> IO ()
runForInput n =
  let
    Just (l, g) = luckyNeighbours n
  in
    if g == n
    then putStrLn $ show n ++ " is a lucky number"
    else putStrLn $ show l ++ " < " ++ show n ++ " < " ++ show g

challenge1 = runForInput 103
challenge2 = runForInput 225
challenge3 = runForInput 997

bonus = runForInput 10000000