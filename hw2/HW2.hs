{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

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

zipFill :: a -> b -> [a] -> [b] -> [(a, b)]
zipFill _ _ [] [] = []
zipFill aDefault bDefault (a: as) [] = (a, bDefault) : zipFill aDefault bDefault as []
zipFill aDefault bDefault [] (b : bs) = (aDefault, b) : zipFill aDefault bDefault [] bs
zipFill aDefault bDefault (a: as) (b : bs) = (a, b) : zipFill aDefault bDefault as bs

data ZipFail = ErrorFirst | ErrorSecond deriving (Eq, Show)
zipFail :: [a] -> [b] -> Either ZipFail [(a, b)]
zipFail [] [] = Right []
zipFail [] _ = Left ErrorFirst
zipFail _ [] = Left ErrorSecond
zipFail (a: as) (b: bs) = case zipFail as bs of
                           Left e -> Left e
                           Right xs -> Right ((a, b) : xs)

unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a, b) (as, bs) -> (a : as, b : bs)) ([], [])

-- Section 4: Knight travels
-- Position (0, 0) is the top-left corner.
data KnightPos = KnightPos {x :: Int, y :: Int} deriving (Show, Eq)
data KnightMove = TopLeft | TopRight | RightTop | RightBottom | BottomRight | BottomLeft | LeftBottom | LeftTop deriving (Enum, Bounded, Show, Eq)
-- Utility to get all knight moves. Don't worry about the implementation of this.
allKnightMoves :: [KnightMove]
allKnightMoves = [minBound .. maxBound]
data Board = Board {width :: Int, height :: Int} deriving (Show, Eq)

tour :: Board -> KnightPos -> Maybe [KnightMove]
tour board start = tour' [start] [] start
  where
    tour' squaresVisited moves current
      | length squaresVisited == width board * height board = Just moves  -- all squares visited
      | otherwise = tryMoves moves squaresVisited current allKnightMoves  -- try all possible moves

    tryMoves _ _ _ [] = Nothing  -- no moves left to try
    tryMoves moves squaresVisited current (m:ms) =
      let nextPos = moveKnight current m
      in if isValidPosition board nextPos && notElem nextPos squaresVisited
         then case tour' (nextPos : squaresVisited) (moves ++ [m]) nextPos of
              Nothing -> tryMoves moves squaresVisited current ms  -- current path failed, try next move
              solution -> solution
         else tryMoves moves squaresVisited current ms  -- move was not valid, try next one


newtype InvalidPosition = InvalidPosition KnightPos deriving (Show, Eq)

translate :: KnightPos -> [KnightMove] -> [KnightPos]
translate _ [] = []
translate start (move:moves) = moveKnight start move : translate (moveKnight start move) moves

translate' :: [KnightPos] -> Either InvalidPosition [KnightMove]
translate' [] = Right []
translate' [_] = Right []
translate' (p1 : p2 : ps) = case find (\m -> p2 `elem` translate p1 [m]) allKnightMoves of
                             Nothing -> Left (InvalidPosition p2)
                             Just m -> case translate' (p2 : ps) of
                                         Left e -> Left e
                                         Right ms -> Right (m : ms)

moveKnight :: KnightPos -> KnightMove -> KnightPos
moveKnight (KnightPos x y) move = case move of
    TopLeft -> KnightPos (x - 2) (y - 1)
    TopRight -> KnightPos (x + 2) (y - 1)
    RightTop -> KnightPos (x + 1) (y - 2)
    RightBottom -> KnightPos (x + 1) (y + 2)
    BottomRight -> KnightPos (x + 2) (y + 1)
    BottomLeft -> KnightPos (x - 2) (y + 1)
    LeftBottom -> KnightPos (x - 1) (y + 2)
    LeftTop -> KnightPos (x - 1) (y - 2)

isValidPosition :: Board -> KnightPos -> Bool
isValidPosition (Board w h) (KnightPos x y) = x >= 0 && x < w && y >= 0 && y < h

-- Bonus (10 points)
mark :: Board -> [KnightPos] -> Either InvalidPosition [[Int]]
mark board positions
  | Just invalidPos <- findFirstInvalid positions = Left $ InvalidPosition invalidPos
  | Just duplicatedPos <- findFirstDuplicate positions = Left $ InvalidPosition duplicatedPos
  | otherwise = Right $ createBoardMarking board positions
  where
    findFirstInvalid :: [KnightPos] -> Maybe KnightPos
    findFirstInvalid [] = Nothing
    findFirstInvalid (p:ps)
      | not (isValidPosition board p) = Just p
      | otherwise = findFirstInvalid ps

    findFirstDuplicate :: [KnightPos] -> Maybe KnightPos
    findFirstDuplicate [] = Nothing
    findFirstDuplicate (p:ps)
      | p `elem` ps = Just p
      | otherwise = findFirstDuplicate ps

    createBoardMarking (Board w h) knightPositions = 
      [[findPositionIndex (KnightPos x y) knightPositions 0 | x <- [0..w-1]] | y <- [0..h-1]]

    findPositionIndex :: KnightPos -> [KnightPos] -> Int -> Int
    findPositionIndex _ [] _ = -1
    findPositionIndex target (p:ps) idx
      | target == p = idx
      | otherwise = findPositionIndex target ps (idx + 1)

