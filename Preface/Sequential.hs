{-# LANGUAGE NoImplicitPrelude #-}

module Preface.Sequential (
  module Preface.Sequential
  ) where

import Prelude (Maybe, Int, Eq, Bool, (!!), id, error, Integral, fromIntegral )
import GHC.Exts (toList)
import qualified Data.Sequence as S
import qualified Data.List as L

class Sequential a where
  (<|) :: b -> a b -> a b
  (><) :: a b -> a b -> a b
  (|>) :: a b -> b -> a b
  elemIndexL :: Eq b => b -> a b -> Maybe Int
  findIndexL :: (b->Bool) -> a b -> Maybe Int
  (!) :: Integral c => a b -> c -> b
  asList :: a b -> [b]
  length :: a b -> Int
  map :: (b -> c) -> a b -> a c
  init :: a b -> a b
  scanl :: (b -> c -> b) -> b -> a c -> a b
  zipWith :: (b -> c -> d) -> a b -> a c -> a d

-- data ViewL a = EmptyL | a :< (Seq a)
-- data ViewR a = EmptyR | (Seq a) :> a

-- adjust ∷ (a → a) → Int → Seq a → Seq a
{-
breakl ∷ (a → Bool) → Seq a → (Seq a, Seq a)
breakr ∷ (a → Bool) → Seq a → (Seq a, Seq a)
Data.Sequence.drop ∷ Int → Seq a → Seq a
dropWhileL ∷ (a → Bool) → Seq a → Seq a
dropWhileR ∷ (a → Bool) → Seq a → Seq a
elemIndexR ∷ Eq a ⇒ a → Seq a → Maybe Int
elemIndicesL ∷ Eq a ⇒ a → Seq a → [Int]
elemIndicesR ∷ Eq a ⇒ a → Seq a → [Int]
empty ∷ Seq a
Data.Sequence.filter ∷ (a → Bool) → Seq a → Seq a
findIndexR ∷ (a → Bool) → Seq a → Maybe Int
findIndicesL ∷ (a → Bool) → Seq a → [Int]
findIndicesR ∷ (a → Bool) → Seq a → [Int]
foldlWithIndex ∷ (b → Int → a → b) → b → Seq a → b
foldrWithIndex ∷ (Int → a → b → b) → b → Seq a → b
fromArray ∷ Ix i ⇒ Array i a → Seq a
fromFunction ∷ Int → (Int → a) → Seq a
Data.Sequence.fromList ∷ [a] → Seq a
Data.Sequence.inits ∷ Seq a → Seq (Seq a)
iterateN ∷ Int → (a → a) → a → Seq a
Data.Sequence.length ∷ Seq a → Int
mapWithIndex ∷ (Int → a → b) → Seq a → Seq b
Data.Sequence.null ∷ Seq a → Bool
Data.Sequence.partition ∷ (a → Bool) → Seq a → (Seq a, Seq a)
Data.Sequence.replicate ∷ Int → a → Seq a
replicateA ∷ Applicative f ⇒ Int → f a → f (Seq a)
Data.Sequence.replicateM ∷ Monad m ⇒ Int → m a → m (Seq a)
Data.Sequence.reverse ∷ Seq a → Seq a
Data.Sequence.scanl1 ∷ (a → a → a) → Seq a → Seq a
Data.Sequence.scanr ∷ (a → b → b) → b → Seq a → Seq b
Data.Sequence.scanr1 ∷ (a → a → a) → Seq a → Seq a
singleton ∷ a → Seq a
Data.Sequence.sort ∷ Ord a ⇒ Seq a → Seq a
Data.Sequence.sortBy ∷ (a → a → Ordering) → Seq a → Seq a
spanl ∷ (a → Bool) → Seq a → (Seq a, Seq a)
spanr ∷ (a → Bool) → Seq a → (Seq a, Seq a)
Data.Sequence.splitAt ∷ Int → Seq a → (Seq a, Seq a)
Data.Sequence.tails ∷ Seq a → Seq (Seq a)
Data.Sequence.take ∷ Int → Seq a → Seq a
takeWhileL ∷ (a → Bool) → Seq a → Seq a
takeWhileR ∷ (a → Bool) → Seq a → Seq a
unfoldl ∷ (b → Maybe (b, a)) → b → Seq a
Data.Sequence.unfoldr ∷ (b → Maybe (a, b)) → b → Seq a
unstableSort ∷ Ord a ⇒ Seq a → Seq a
unstableSortBy ∷ (a → a → Ordering) → Seq a → Seq a
update ∷ Int → a → Seq a → Seq a
viewl ∷ Seq a → ViewL a
viewr ∷ Seq a → ViewR a
Data.Sequence.zip ∷ Seq a → Seq b → Seq (a, b)
Data.Sequence.zip3 ∷ Seq a → Seq b → Seq c → Seq (a, b, c)
Data.Sequence.zip4 ∷
  Seq a → Seq b → Seq c → Seq d → Seq (a, b, c, d)
Data.Sequence.zipWith3 ∷
  (a → b → c → d) → Seq a → Seq b → Seq c → Seq d
zipWith4 ∷
  (a → b → c → d → e) → Seq a → Seq b → Seq c → Seq d → Seq e
-}

instance Sequential [] where
  (<|) = (:)
  (><) = (L.++)
  (|>) c d = (L.++) c (d:[])
  elemIndexL = L.elemIndex
  findIndexL = L.findIndex
  (!) x y = (!!) x ( fromIntegral y)
  asList = id 
  length = L.length
  map = L.map
  init = L.init
  scanl = L.scanl
  zipWith = L.zipWith

instance Sequential S.Seq where
  (<|) = (S.<|)
  (><) = (S.><)
  (|>) = (S.|>)
  elemIndexL = S.elemIndexL
  findIndexL = S.findIndexL
  (!) x y = S.index x (fromIntegral y)
  asList = toList
  length = S.length
  map f = S.mapWithIndex (\_x y -> f y)
  init x = case S.viewr x of 
     S.EmptyR -> error "Sequential.init: empty list"
     a S.:> _b -> a
  scanl = S.scanl
  zipWith = S.zipWith
{-
instance (Ix a, Num a) => Sequential (A.Array a) where
  (<|) = undefined
  (!) a b = (A.!) a (fromIntegral b)
  asList = elems
  length = A.numElements
-}

type Seq = S.Seq

