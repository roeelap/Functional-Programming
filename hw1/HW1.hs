-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --
-- Section 1
-- ********* --

const :: a -> b -> a
const x _ = x

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) f g a = g (f a)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f c a b = f a b c

lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f b c a = f a b c

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g a b c = f (g a b c)

(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) f g a b c d = f (g a b c d)

(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) f g a b c d e = f (g a b c d e)

(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) f' g' a b c d e f  = f' (g' a b c d e f)

-- How can we ever implement such a function!?
impossible :: a -> b
impossible = error "impossible"

-- ********* --
-- Section 2
-- ********* --

countDigits :: Integer -> Integer
countDigits x = if x < 10 && -10 < x then 1 else 1 + countDigits (x `div` 10)

toBinary :: Integer -> Integer
toBinary x
    | x < 0     = (-1) * toBinary (-x)
    | x < 2     = x
    | otherwise = (x `mod` 2) + 10 * toBinary (x `div` 2)

fromBinary :: Integer -> Integer
fromBinary x
    | x < 0     = (-1) * fromBinary (-x)
    | x < 2     = x
    | otherwise = (x `mod` 10) + 2 * fromBinary (x `div` 10)

isAbundant :: Integer -> Bool
isAbundant x
    | x < 0 = False
    | otherwise = x < sumDivisors x (x `div` 2)
    where sumDivisors _ 0 = 0
          sumDivisors z y = if z `mod` y == 0 then y + sumDivisors z (y - 1) else sumDivisors z (y - 1)

rotateDigits :: Integer -> Integer
rotateDigits x
    | x < 10 && -10 < x = x
    | x < 0 = (-1) * rotateRight (-x)
    | otherwise = rotateLeft x
    where
        rotateRight n = (n `mod` 10) * (10 `power` (countDigits n - 1)) + n `div` 10
        rotateLeft n = n `div` (10 `power` (countDigits n - 1)) + (n `mod` (10 `power` (countDigits n - 1))) * 10

power :: Integer -> Integer -> Integer
power x y = if y == 0 then 1 else x * power x (y - 1)

-- -- ********* --
-- -- Section 3
-- -- ********* --
type Generator a = (a -> a, a -> Bool, a)

nullGen :: Generator a -> Bool
nullGen (_, p, x) = not (p x)

lastGen :: Generator a -> a
lastGen (f, p, x) = if not (p x) then x else lastGen (f, p, f x)

lengthGen :: Generator a -> Int
lengthGen (f, p, x) = if not (p x) then 0 else 1 + lengthGen (f, p, f x)

sumGen :: Generator Integer -> Integer
sumGen (f, p, x) = if not (p x) then 0 else f x + sumGen (f, p, f x)

type Predicate a = a -> Bool
anyGen :: Predicate a -> Generator a -> Bool
anyGen p (f, q, x) = q x && (p (f x) || anyGen p (f, q, f x))

allGen :: Predicate a -> Generator a -> Bool
allGen p (f, q, x) = not (q x) || (p (f x) && allGen p (f, q, f x))

noneGen :: Predicate a -> Generator a -> Bool
noneGen p (f, q, x) = not (q x) || (not (p (f x)) && noneGen p (f, q, f x))

countGen :: Predicate a -> Generator a -> Int
countGen p (f, q, x)
  | not (q x) = 0
  | p (f x) = 1 + countGen p (f, q, f x)
  | otherwise = countGen p (f, q, f x)

-- -- ********* --
-- -- Section 4
-- -- ********* --
isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = isPrimeHelper n 3

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper n divisor
    | divisor * divisor > n = True
    | n `mod` divisor == 0 = False
    | otherwise = isPrimeHelper n (divisor + 2)

isSemiprime :: Integer -> Bool
isSemiprime n = isSemiprimeHelper n 2

isSemiprimeHelper :: Integer -> Integer -> Bool
isSemiprimeHelper n divisor
    | divisor * divisor > n = False
    | n `mod` divisor == 0 && isPrime divisor && isPrime (n `div` divisor) = True
    | otherwise = isSemiprimeHelper n (divisor + 1)

goldbachPair :: Integer -> (Integer, Integer)
goldbachPair x = findGoldbachPair x 2
    where findGoldbachPair z y
            | isPrime y && isPrime (z - y) = (y, z - y)
            | otherwise = findGoldbachPair z (y + 1)

goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' x = findGoldbachPairWithMaxProduct x 2 2
    where findGoldbachPairWithMaxProduct z y maxY
            | y > z - 2 = if maxY > z - maxY then (maxY, z - maxY) else (x - maxY, maxY)
            | y * (z - y) < (maxY * (z - maxY)) = findGoldbachPairWithMaxProduct z (y + 1) maxY
            | isPrime y && isPrime (z - y) = findGoldbachPairWithMaxProduct z (y + 1) y
            | otherwise = findGoldbachPairWithMaxProduct z (y + 1) maxY


-- ***** --
-- Bonus
-- ***** --
isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime x
    | x < 2 = False
    | containsZero x = False
    | otherwise = checkRotationsForPrimality x (countDigits x - 1)

-- Checks if a number contains the digit '0'
containsZero :: Integer -> Bool
containsZero x
    | x < 0 = containsZero (-x)
    | x == 0 = True
    | otherwise = checkZero x
  where
    checkZero n
        | n == 0 = False
        | n `mod` 10 == 0 = True
        | otherwise = checkZero (n `div` 10)

-- Checks if all rotations of a number are prime
checkRotationsForPrimality :: Integer -> Integer -> Bool
checkRotationsForPrimality x 0 = isPrime x
checkRotationsForPrimality x count = isPrime x && checkRotationsForPrimality (rotateDigits x) (count - 1)
