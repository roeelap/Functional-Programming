-- {-# LANGUAGE LambdaCase #-}
-- {-# OPTIONS_GHC -Wall -Werror #-}
-- {-# OPTIONS_GHC -Wno-unused-imports #-}

module EqMap (
  EqMap,
  CombiningMap (..),
  empty,
  EqMap.insert, -- To avoid name clash with Data.List.insert
  member,
  remove,
  EqMap.lookup, -- To avoid name clash with Prelude.lookup
  assocs
) where

import Data.Either
import Data.List
import Data.Maybe
import Data.Semigroup (Arg (..))
import EqSet (EqSet)
import EqSet qualified

newtype EqMap k v = EqMap { getMap :: EqSet (Arg k v) }

empty :: EqMap k v
empty = EqMap EqSet.empty

member :: Eq k => k -> EqMap k v -> Bool
member k (EqMap kvs) = any (\(Arg k' _) -> k == k') (EqSet.elems kvs)

insert :: Eq k => k -> v -> EqMap k v -> EqMap k v
insert k v (EqMap kvs) = EqMap (EqSet.insert (Arg k v) (removeKey k kvs)) 

removeKey :: Eq k => k -> EqSet (Arg k v) -> EqSet (Arg k v)
removeKey k = EqSet.filter (\(Arg k' _) -> k == k)

remove :: Eq k => k -> EqMap k v -> EqMap k v
remove k (EqMap kvs) = EqMap (removeKey k kvs)

lookup :: Eq k => k -> EqMap k v -> Maybe v
lookup k (EqMap kvs) = case find (\(Arg k' v) -> k == k') (EqSet.elems kvs) of
  Just (Arg _ v) -> Just v
  Nothing -> Nothing

assocs :: EqMap k v -> [(k, v)]
assocs (EqMap es) = map (\(Arg k v) -> (k, v)) (EqSet.elems es)

instance (Eq k, Eq v) => Eq (EqMap k v) where
  (EqMap kvs1) == (EqMap kvs2) = kvs1 == kvs2

instance (Show k, Show v) => Show (EqMap k v) where
  show (EqMap kvs) = "{" ++ showAssocs (EqSet.elems kvs) ++ "}"
    where
      showAssocs [] = ""
      showAssocs [Arg k v] = show k ++ "->" ++ show v
      showAssocs (Arg k v : rest) = show k ++ "->" ++ show v ++ "," ++ showAssocs rest

instance Eq k => Semigroup (EqMap k v) where
  (EqMap es1) <> (EqMap es2) = EqMap (combine es2 (EqSet.elems es1))
    where
      combine es [] = es
      combine es (arg@(Arg k v) : rest) = combine (EqSet.insert arg (removeKey k es)) rest
  
instance Eq k => Monoid (EqMap k v) where
  mempty = empty
  mappend = (<>)

newtype CombiningMap k v = CombiningMap {getCombiningMap :: EqMap k v}

instance (Eq k, Semigroup v) => Semigroup (CombiningMap k v) where
  (CombiningMap (EqMap es1)) <> (CombiningMap (EqMap es2)) = CombiningMap $ EqMap (combine es1 (EqSet.elems es2))
    where
      combine :: EqSet (Arg k v) -> [Arg k v] -> EqSet (Arg k v)
      combine es [] = es
      combine es (Arg k v : rest) =
        let updatedSet = case EqMap.lookup k (EqMap es) of
                           Just v' -> EqSet.insert (Arg k (v' <> v)) (removeKey k es)
                           Nothing -> EqSet.insert (Arg k v) es
        in combine updatedSet rest

instance (Eq k, Semigroup v) => Monoid (CombiningMap k v) where
  mempty = CombiningMap empty
  mappend = (<>)


map1 = EqMap.insert 1 "a" $ EqMap.insert 2 "b" empty
map2 = EqMap.insert 2 "c" $ EqMap.insert 3 "d" empty

-- >>> getCombiningMap $ (CombiningMap map1) <> (CombiningMap map2)
-- {3->"d",1->"a",2->"b"}

