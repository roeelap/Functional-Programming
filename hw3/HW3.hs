{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW3.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW3 where

import Prelude (Bool (..), Char, Either (..), Enum (..), Eq (..), Int, Integer, Maybe (..), Num (..), Ord (..), Rational, Show (..), String, all, and, any, concat, concatMap, const, curry, div, drop, dropWhile, elem, error, even, filter, flip, foldr, fst, id, init, last, length, lines, lookup, map, maximum, minimum, mod, not, notElem, null, odd, or, otherwise, product, reverse, snd, splitAt, sum, tail, take, takeWhile, uncurry, undefined, unlines, unwords, unzip, words, zip, zipWith, (!!), ($), (&&), (++), (.), (||))

import Data.Either (either, fromLeft, fromRight, isLeft, isRight, lefts, partitionEithers, rights)
import Data.List (find, foldl', isInfixOf, isPrefixOf, isSuffixOf, nub, uncons)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybe)
import Data.Ratio (denominator, numerator, (%))
import Text.Read (readMaybe)
import Control.Arrow (ArrowChoice(right))

-- Section 1: Tree Serialization
data Tree a = Empty | Tree (Tree a) a (Tree a) deriving (Show, Eq)

serialize :: Tree Int -> [Int]
serialize Empty = []
serialize (Tree Empty v Empty) = [v, 0] -- 0 meaning no children
serialize (Tree l v Empty) = [v, 1] ++ serialize l -- 1 meaning has only left child
serialize (Tree Empty v r) = [v, 3] ++ serialize r -- 3 meaning has only right child
serialize (Tree l v r) = [v, 2] ++ serialize l ++ serialize r -- 2 meaning has both children

deserialize :: [Int] -> Tree Int
deserialize xs = fst (deserializeHelper xs)

deserializeHelper :: [Int] -> (Tree Int, [Int])
deserializeHelper = \case
    [] -> (Empty, [])
    (val:0:xs) -> (Tree Empty val Empty, xs)
    (val:1:xs) ->
        let (l, rest) = deserializeHelper xs
        in (Tree l val Empty, rest)
    (val:3:xs) ->
        let (r, rest) = deserializeHelper xs
        in (Tree Empty val r, rest)
    (val:2:xs) ->
        let (l, rest1) = deserializeHelper xs
            (r, rest2) = deserializeHelper rest1
        in (Tree l val r, rest2)
    _ -> error "Invalid input"

-- Section 2: Infinite lists
data InfiniteList a = a :> InfiniteList a
infixr 5 :>

sample :: InfiniteList a -> [a]
sample = take 10 . itoList

smallSample :: InfiniteList a -> [a]
smallSample = take 5 . itoList

itoList :: InfiniteList a -> [a]
itoList (x :> xs) = x : itoList xs

iiterate :: (a -> a) -> a -> InfiniteList a
iiterate f x = x :> iiterate f (f x)

irepeat :: a -> InfiniteList a
irepeat = iiterate id

iprepend :: [a] -> InfiniteList a -> InfiniteList a
iprepend xs ys = foldr (:>) ys xs

itake :: Integer -> InfiniteList a -> [a]
itake n (x :> xs)
    | n <= 0 = []
    | otherwise = x : itake (n - 1) xs

idrop :: Integer -> InfiniteList a -> InfiniteList a
idrop n (x :> xs)
    | n <= 0 = x :> xs
    | otherwise = idrop (n - 1) xs

naturals :: InfiniteList Integer
naturals = iiterate (+ 1) 0

imap :: (a -> b) -> InfiniteList a -> InfiniteList b
imap f (x :> xs) = f x :> imap f xs

ifilter :: (a -> Bool) -> InfiniteList a -> InfiniteList a
ifilter f (x :> xs)
    | f x = x :> ifilter f xs
    | otherwise = ifilter f xs

ifind :: (a -> Bool) -> InfiniteList a -> a
ifind f (x :> xs)
    | f x = x
    | otherwise = ifind f xs

iconcat :: InfiniteList [a] -> InfiniteList a
iconcat (x :> xs) = foldr (:>) (iconcat xs) x

integers :: InfiniteList Integer
integers = imap (\x -> if even x then x `div` 2 else -((x + 1) `div` 2)) naturals

rationals :: InfiniteList Rational
rationals = interleave positiveFractions negativeFractions where
    positiveFractions = generateFractions (1 % 1)
    negativeFractions = imap negate positiveFractions
    
    generateFractions :: Rational -> InfiniteList Rational
    generateFractions r = r :> interleave (generateFractions (updateDenominator r)) (generateFractions (updateNominator r))
    
    updateDenominator :: Rational -> Rational
    updateDenominator r = numerator r % (numerator r + denominator r)
    
    updateNominator :: Rational -> Rational
    updateNominator r = (numerator r + denominator r) % denominator r
    
    interleave :: InfiniteList a -> InfiniteList a -> InfiniteList a
    interleave (x :> xs) ys = x :> interleave ys xs

-- Bonus: same as rationals, but without repeats!
rationals' :: InfiniteList Rational
rationals' = rationals

-- Section 3: Stack Machine
data StackError = DivisionByZero | StackUnderflow {instruction :: String, stackValue :: Maybe Int} deriving (Show, Eq)

data RunError = InstructionError StackError | ParseError {line :: String} deriving (Show, Eq)

data Instruction = PUSH Int | POP | SWAP | DUP | ADD | SUB | MUL | DIV | NOTHING
    deriving (Show, Eq)

readInt :: String -> Maybe Int
readInt = readMaybe

parseInstruction :: String -> Maybe Instruction
parseInstruction line =
    case words line of
        [] -> Just NOTHING
        ["PUSH", n] -> case readInt n of
            Nothing -> Nothing
            Just n' -> Just (PUSH n')
        ["POP"] -> Just POP
        ["SWAP"] -> Just SWAP
        ["DUP"] -> Just DUP
        ["ADD"] -> Just ADD
        ["SUB"] -> Just SUB
        ["MUL"] -> Just MUL
        ["DIV"] -> Just DIV
        _ -> Nothing  -- Invalid instruction format

runInstruction :: [Int] -> Instruction -> Either StackError [Int]
runInstruction stack (PUSH n) = Right (n : stack)

runInstruction [] POP = Left $ StackUnderflow "POP" Nothing
runInstruction (_ : xs) POP = Right xs

runInstruction [] SWAP = Left $ StackUnderflow "SWAP" Nothing
runInstruction [x] SWAP = Left $ StackUnderflow "SWAP" (Just x)
runInstruction (x : y : xs) SWAP = Right (y : x : xs)

runInstruction [] DUP = Left $ StackUnderflow "DUP" Nothing
runInstruction (x : xs) DUP = Right (x : x : xs)

runInstruction [] ADD = Left $ StackUnderflow "ADD" Nothing
runInstruction [x] ADD = Left $ StackUnderflow "ADD" (Just x)
runInstruction (x : y : xs) ADD = Right (x + y : xs)

runInstruction [] SUB = Left $ StackUnderflow "SUB" Nothing
runInstruction [x] SUB = Left $ StackUnderflow "SUB" (Just x)
runInstruction (x : y : xs) SUB = Right (x - y : xs)

runInstruction [] MUL = Left $ StackUnderflow "MUL" Nothing
runInstruction [x] MUL = Left $ StackUnderflow "MUL" (Just x)
runInstruction (x : y : xs) MUL = Right (x * y : xs)

runInstruction [] DIV = Left $ StackUnderflow "DIV" Nothing
runInstruction [x] DIV = Left $ StackUnderflow "DIV" (Just x)
runInstruction (_ : 0 : _) DIV = Left DivisionByZero
runInstruction (x : y : xs) DIV = Right (x `div` y : xs)

runInstruction stack NOTHING = Right stack

parseAndRun :: String -> Either RunError [Int]
parseAndRun input = runInstructions [] (lines input) where
    runInstructions :: [Int] -> [String] -> Either RunError [Int]
    runInstructions stack [] = Right stack
    runInstructions stack (l : ls) = case parseInstruction l of
        Nothing -> Left $ ParseError l
        Just instruction -> case runInstruction stack instruction of
            Left err -> Left $ InstructionError err
            Right stack' -> runInstructions stack' ls
