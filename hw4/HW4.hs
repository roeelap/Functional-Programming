{-# LANGUAGE LambdaCase #-}
-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW4.hs EqSet.hs EqMap.hs should successfully compile.
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}

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
  serialize Nothing = [0]
  serialize (Just x) = 1 : serialize x
  deserialize (0:_) = Nothing
  deserialize (1:xs) = Just (deserialize xs)
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
  serialize xs = serialize (length xs) ++ concatMap serialize xs
  deserialize xs =
    let (lenSerialized, rest) = splitAt 1 xs
        len = deserialize lenSerialized :: Int
        takeElements 0 [] = []
        takeElements n lst = 
          let (element, remaining) = splitAt (length (serialize (undefined :: a))) lst
          in deserialize element : takeElements (n - 1) remaining
    in takeElements len rest

instance (Serializable a, Eq a) => Serializable (EqSet a) where
  serialize s = serialize (length elements) ++ concatMap serialize elements
    where elements = toList s

  deserialize xs =
    let (lenSerialized, rest) = splitAt 1 xs
        len = deserialize lenSerialized :: Int
        takeElements 0 [] = []
        takeElements n lst = 
          let (element, remaining) = splitAt (length (serialize (undefined :: a))) lst
          in deserialize element : takeElements (n - 1) remaining
    in fromList (takeElements len rest)

toList :: EqSet a -> [a]
toList = EqSet.elems

fromList :: Eq a => [a] -> EqSet a
fromList = foldr EqSet.insert EqSet.empty

instance (Serializable k, Eq k, Serializable v) => Serializable (EqMap k v) where
  serialize m = serialize (length keys) ++ concatMap serialize keys ++ concatMap serialize values
    where
      (keys, values) = unzip (mapToList m)

  deserialize xs =
    let (lenSerialized, rest) = splitAt 1 xs
        len = deserialize lenSerialized :: Int
        (keysSerialized, valuesSerialized) = splitAt (len * length (serialize (undefined :: k))) rest
        takeElements 0 _ = []
        takeElements n lst = 
          let (element, remaining) = splitAt (length (serialize (undefined :: k))) lst
          in deserialize element : takeElements (n - 1) remaining
        keys = takeElements len keysSerialized
        values = takeElements len valuesSerialized
    in EqMap.toMap keys values

mapToList :: EqMap k v -> [(k, v)]
mapToList = EqMap.assocs

-- Section 3: Metric
infinity :: Double
infinity = 1 / 0

class Eq a => Metric a where
  distance :: a -> a -> Double

instance Metric Double where
  distance :: Double -> Double -> Double
  distance x y = abs (x - y)

instance Metric Int where
  distance :: Int -> Int -> Double
  distance x y = fromIntegral (abs (x - y))

instance Metric Char where
  distance :: Char -> Char -> Double
  distance x y = fromIntegral (abs (ord x - ord y))

-- Euclidean distance
instance (Metric a, Metric b) => Metric (a, b) where
  distance :: (a, b) -> (a, b) -> Double
  distance (x1, y1) (x2, y2) = sqrt (distance x1 x2 ^ (2 :: Integer) + distance y1 y2 ^ (2 :: Integer))

data ManhattanTuple a b = ManhattanTuple a b deriving Eq

instance (Metric a, Metric b) => Metric (ManhattanTuple a b) where
  distance :: ManhattanTuple a b -> ManhattanTuple a b -> Double
  distance (ManhattanTuple x1 y1) (ManhattanTuple x2 y2) = distance x1 x2 + distance y1 y2

-- Just and Nothing have distance of infinity.
-- Two Justs measure the distance between the two values.
instance Metric a => Metric (Maybe a) where
  distance :: Maybe a -> Maybe a -> Double
  distance Nothing Nothing = 0
  distance (Just x) (Just y) = distance x y
  distance _ _ = infinity

-- Left and Right have a distance of infinity.
-- Same constructores measure the distance between the two values.
instance (Metric a, Metric b) => Metric (Either a b) where
  distance :: Either a b -> Either a b -> Double
  distance (Left x) (Left y) = distance x y
  distance (Right x) (Right y) = distance x y
  distance _ _ = infinity

-- Lists of different sizes have distance of infinity.
-- Euclidean distance.
instance Metric a => Metric [a] where
  distance :: [a] -> [a] -> Double
  distance xs ys
    | length xs /= length ys = infinity
    | otherwise = sqrt . sum $ zipWith (\x y -> distance x y ^ (2 :: Integer)) xs ys

newtype ManhattanList a = ManhattanList [a] deriving Eq

instance Metric a => Metric (ManhattanList a) where
  distance :: ManhattanList a -> ManhattanList a -> Double
  distance (ManhattanList xs) (ManhattanList ys)
    | length xs /= length ys = infinity
    | otherwise = sum $ zipWith distance xs ys

-- Returns the element with the shortest distance to the input.
-- If there are no numbers whose distance is less than infinity, return Nothing.
closest :: Metric a => a -> [a] -> Maybe a
closest = closestOn id

-- Similar to the above, but uses a function move the element
-- to another metric space.
closestOn :: Metric b => (a -> b) -> a -> [a] -> Maybe a
closestOn f x xs = case filter (\y -> distance (f x) (f y) < infinity) xs of
  [] -> Nothing
  ys -> Just $ minimumBy (comparing' (distance (f x) . f)) ys

comparing' :: Ord b => (a -> b) -> a -> a -> Ordering
comparing' f x y = compare (f x) (f y)

-- Will not swap elements whose distance is less than d, even if their
-- order implies they should be swapped.
metricBubbleSort :: (Metric a, Ord a) => Double -> [a] -> [a]
metricBubbleSort = metricBubbleSortOn id

-- Similar to the above, but uses a function to extract the value used for sorting.
metricBubbleSortOn :: (Metric b, Ord b) => (a -> b) -> Double -> [a] -> [a]
metricBubbleSortOn f d xs = foldr bubble xs [1..length xs]
  where
    bubble _ [] = []
    bubble _ [y] = [y]
    bubble _ (y1:y2:ys)
      | f y1 > f y2 && distance (f y1) (f y2) >= d = y2 : bubble 0 (y1 : ys)
      | otherwise = y1 : bubble 0 (y2 : ys)

-- Bonus (10 points).
clusters :: Metric a => [a] -> [[a]]
clusters xs = clusterHelper xs []

-- Function to group elements with finite distances into clusters
clusterHelper :: Metric a => [a] -> [[a]] -> [[a]]
clusterHelper [] acc = acc
clusterHelper (x:xs) acc =
  let (cluster, remaining) = partition (\y -> distance x y < infinity) xs
  in clusterHelper remaining ((x:cluster) : acc)

-- Helper function to partition the list
partition' :: (a -> Bool) -> [a] -> ([a], [a])
partition' p xs = (filter p xs, filter (not . p) xs)
