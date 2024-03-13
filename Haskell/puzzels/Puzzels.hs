module Puzzels where
import Data.Foldable (Foldable)
-- import Puzzels (length')

length' :: [Int] -> Int
length' [] = 0
length' x = foldr (\_ acc -> acc + 1) 0 x

or' :: [Bool] -> Bool
or' [] = False
or' x = foldr (||) False x

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x list = foldr (\a acc -> (a == x) || acc) False list

main = do
    putStrLn "Ran main"
