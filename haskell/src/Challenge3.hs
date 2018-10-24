module Challenge3 where

import Control.Applicative
import Data.List
import qualified Data.Set as S

ducci :: (Num a, Ord a) => [a] -> [[a]]
ducci as = takeUntilDuplicate $ iterate ducciStep as

takeUntilDuplicate :: Ord a => [a] -> [a]
takeUntilDuplicate xs = foldr go (const []) xs S.empty
  where
    go :: Ord t => t -> (S.Set t -> [t]) -> S.Set t -> [t]
    go x cont set
      | x `S.member` set = []
      | otherwise      = x : cont (S.insert x set)

ducciStep :: Num a => [a] -> [a]
ducciStep as = difference <$> pairs as

pairs :: [a] -> [(a, a)]
pairs as = zip as $ tail $ cycle as

difference :: Num a => (a, a) -> a
difference (a1, a2) = abs $ a1 - a2

formatDucci :: Show a => [[a]] -> String
formatDucci lines = intercalate "\n" (fmap formatLine lines) ++ "\n" ++ show (length lines) ++ " Steps"
  where
    formatLine line = "[" ++ intercalate "; " (fmap show line) ++ "]"


run :: (Show a, Num a, Ord a) => [a] -> IO ()
run input = putStrLn $ formatDucci $ ducci input

runSample = run [0, 653, 1854, 4063]

runChallenge1 = run [1, 5, 7, 9, 9]

runChallenge2 = run [1, 2, 1, 2, 1, 0]

runChallenge3 = run [10, 12, 41, 62, 31, 50]

runChallenge4 = run [10, 12, 41, 62, 31]