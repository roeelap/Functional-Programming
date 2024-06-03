-- {-# LANGUAGE LambdaCase #-}
-- -- Implement the following functions.
-- -- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- -- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- -- eval (-- >>>) won't work.
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- -- Refines the above, allowing for unused imports.
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module HW4 where

import Data.Char (chr, ord)
import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqMap (EqMap)
import EqMap qualified
import EqSet (EqSet)
import EqSet qualified

-- Section 2: Serialization
class Serializable a where
  serialize :: a -> [Int]
  deserialize :: [Int] -> a

instance Serializable Int where
  serialize x = [x]
  deserialize [x] = x
  deserialize _ = error "Invalid input"

instance Serializable Bool where
  serialize True = [1]
  serialize False = [0]
  deserialize [1] = True
  deserialize [0] = False
  deserialize _ = error "Invalid input"

instance Serializable Char where
  serialize x = [ord x]
  deserialize [x] = chr x
  deserialize _ = error "Invalid input"

instance Serializable a => Serializable (Maybe a) where
  serialize Nothing = [0, 0]
  serialize (Just x) = 1 : serialize x
  deserialize (0:_) = Nothing
  deserialize (1:xs) = deserialize xs
  deserialize _ = error "Invalid input"

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (x, y) = let sx = serialize x
                         sy = serialize y
                     in serialize (length sx) ++ sx ++ serialize (length sy) ++ sy
  deserialize xs = let (len1, rest1) = splitAt 1 xs
                       (xs1, rest2) = splitAt (deserialize len1) rest1
                       (len2, xs2) = splitAt 1 rest2
                       ys = take (deserialize len2) xs2
                   in (deserialize xs1, deserialize ys)
  
instance (Serializable a, Serializable b) => Serializable (Either a b) where
  serialize (Left x) = 0 : serialize x
  serialize (Right y) = 1 : serialize y
  deserialize (0:xs) = Left (deserialize xs)
  deserialize (1:xs) = Right (deserialize xs)
  deserialize _ = error "Invalid input"

instance Serializable a => Serializable [a] where



instance (Serializable a, Eq a) => Serializable (EqSet a) where
  


instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v)

-- -- Section 3: Metric
-- infinity :: Double
-- infinity = 1 / 0

-- class Eq a => Metric a where
--   distance :: a -> a -> Double

-- instance Metric Double where
--   distance = (-) `on` abs

-- instance Metric Int where
--   distance = fromIntegral . abs . (-)

-- instance Metric Char where
--   distance = fromIntegral . abs . (-) `on` ord

-- -- Euclidean distance
-- instance (Metric a, Metric b) => Metric (a, b) where
--   distance (x1, y1) (x2, y2) = sqrt (distance x1 x2 ^ 2 + distance y1 y2 ^ 2)

-- data ManhattanTuple a b = ManhattanTuple a b deriving Eq
-- instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
--   distance (ManhattanTuple x1 y1) (ManhattanTuple x2 y2) = distance x1 x2 + distance y1 y2

-- -- Just and Nothing have distance of infinity.
-- -- Two Justs measure the distance between the two values.
-- instance Metric a => Metric (Maybe a) where
--   distance Nothing Nothing = 0
--   distance (Just x) (Just y) = distance x y
--   distance _ _ = infinity

-- -- Left and Right have a distance of infinity.
-- -- Same constructors measure the distance between the two values.
-- instance (Metric a, Metric b) => Metric (Either a b) where
--   distance (Left x) (Left y) = distance x y
--   distance (Right x) (Right y) = distance x y
--   distance _ _ = infinity

-- -- Lists of different sizes have distance of infinity.
-- -- Euclidean distance.
-- instance Metric a => Metric [a] where
--   distance xs ys
--     | length xs != length ys = infinity
--     | otherwise = sqrt (sum (zipWith (\x y -> distance x y ^ 2) xs ys))

-- newtype ManhattanList a = ManhattanList [a] deriving Eq
-- instance Metric a => Metric (ManhattanList a) where
--   distance (ManhattanList xs) (ManhattanList ys) 
--     | length xs != length ys = infinity
--     | otherwise = sum (zipWith distance xs ys)

-- -- Returns the element with the shortest distance to the input.
-- -- If there are no numbers whose distance is less than infinity, return Nothing.
-- closest :: Metric a => a -> [a] -> Maybe a
-- closest x [] = Nothing
-- closest x [y] = Just $ distance x y

-- -- Similar to the above, but uses a function move the element
-- -- to another metric space.
-- closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
-- -- Will not swap elements whose distance is less than d, even if their
-- -- order implies they should be swapped.
-- metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
-- -- Similar to the above, but uses a function to extract the value used for sorting.
-- metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]

-- -- Bonus (10 points).
-- clusters :: Metric a => [a] -> [[a]]
