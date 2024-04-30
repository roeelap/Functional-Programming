-- {-# LANGUAGE LambdaCase #-}
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module HW2 where

import Data.List (find, foldl', zip)
import Prelude (Bool (..), Bounded (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Show (..), String, all, and, any, concat, concatMap, const, curry, div, elem, error, even, filter, flip, foldl, foldr, fst, id, length, lines, lookup, map, mod, not, notElem, null, odd, otherwise, product, snd, sum, uncurry, undefined, unlines, unwords, words, (!!), ($), (&&), (++), (.), (||))
import Language.Haskell.TH.PprLib (cat)
import Data.Bits (Bits(rotate))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- Section 1.1: Basic Maybes
concatMaybeMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMaybeMap f = \case
  Nothing -> Nothing
  Just x -> f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x = \case
  Nothing -> x
  Just y -> y

maybe :: b -> (a -> b) -> Maybe a -> b
maybe x f = \case
  Nothing -> x
  Just y -> f y

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> maybe acc (: acc) x) []

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- -- Section 1.2 Basic Eithers
concatEitherMap :: (a -> Either e b) -> Either e a -> Either e b
concatEitherMap f = \case
  Left x -> Left x
  Right y -> f y

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g = \case
  Left x -> f x
  Right y -> g y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

catEithers :: [Either e a] -> Either e [a]
catEithers = foldr (\x acc -> case x of
                                Left e -> Left e
                                Right v -> case acc of
                                              Left e -> Left e
                                              Right xs -> Right (v : xs)
                   ) (Right [])

mapEither :: (a -> Either e b) -> [a] -> Either e [b]
mapEither f = foldr (\x acc -> case f x of
                                 Left e -> Left e
                                 Right v -> case acc of
                                               Left e -> Left e
                                               Right xs -> Right (v : xs)
                    ) (Right [])

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (\x (ls, rs) -> case x of
                                          Left l -> (l : ls, rs)
                                          Right r -> (ls, r : rs)
                         ) ([], [])

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- -- Section 2: Lists
take :: Int -> [a] -> [a]
take n = \case
  [] -> []
  x : xs -> if n <= 0 then [] else x : take (n - 1) xs

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = \case
  [] -> []
  x : xs -> if p x then x : takeWhile p xs else []


drop :: Int -> [a] -> [a]
drop n = \case
  [] -> []
  x : xs -> if n <= 0 then x : xs else drop (n - 1) xs

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p = \case
  [] -> []
  x : xs -> if p x then dropWhile p xs else x : xs

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

rotate :: Int -> [a] -> [a]
rotate n xs
    | n <= 0 = xs
    | otherwise = drop n' xs ++ take n' xs where
        n' = length xs - (n `mod` length xs)


lotate :: Int -> [a] -> [a]
lotate n xs
    | n <= 0 = xs
    | otherwise = drop n' xs ++ take n' xs where
        n' = n `mod` length xs

type Generator a = (a -> a, a -> Bool, a)
fromGenerator :: Generator a -> [a]
fromGenerator (f, p, x) = if p x then f x : fromGenerator (f, p, f x) else []

replicate :: Int -> a -> [a]
replicate n x = take n (fromGenerator (const x, const True, x))

-- >>> zipWith (+) [1 , 2 , 3] [4 , 5 , 6]
-- [5,7,9]

inits :: [a] -> [[a]]
inits [] = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

tails :: [a] -> [[a]]
tails [] = [[]]
tails (x : xs) = (x : xs) : tails xs

-- -- Section 3: zips and products
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (a: as) (b:bs) = f a b : zipWith f as bs

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (,)

-- zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
-- data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
-- zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
-- unzip :: [(a, b)] -> ([a], [b])

-- -- Section 4: Knight travels
-- -- Position (0, 0) is the top-left corner.
-- data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
-- data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- -- Utility to get all knight moves. Don't worry about the implementation of this.
-- allKnightMoves :: [KnightMove]
-- allKnightMoves = [minBound .. maxBound]
-- data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)
-- tour :: Board -> KnightPos -> Maybe [KnightMove]
-- newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)
-- translate :: KnightPos -> [KnightMove] -> [KnightPos]
-- translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]

-- -- Bonus (10 points)
-- mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
