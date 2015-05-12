{-# LANGUAGE NoMonomorphismRestriction, RankNTypes, KindSignatures #-}

{-| Unicode versions of common functions
 -}
module Preface.Symbols where

import Data.Set
import Data.List (intersect)
import Data.Bits

-- | Division
-- DIVISION SIGN
-- Unicode: U+00F7, UTF-8: C3 B7
(÷) :: Fractional a => a -> a -> a
(÷) = (/)

-- | Rho (size / resize) 
-- APL FUNCTIONAL SYMBOL RHO
-- Unicode: U+2374, UTF-8: E2 8D B4
(⍴) :: Int -> [a] -> [a]
(⍴) = take

-- | Multiplication
-- MULTIPLICATION SIGN
-- Unicode: U+00D7, UTF-8: C3 97
(×) :: Num a => a -> a -> a
(×) = (*)

-- | Set union
-- UNION
-- Unicode: U+222A, UTF-8: E2 88 AA
(∪) :: Ord a => Set a -> Set a -> Set a
(∪) = union

-- | Set intersection
-- INTERSECTION
-- Unicode: U+2229, UTF-8: E2 88 A9
(∩) :: Eq a => [a] -> [a] -> [a]
(∩) = intersect

-- | Set membership
-- CONTAINS AS MEMBER
-- Unicode: U+220B, UTF-8: E2 88 8B
(∋) :: Ord a => Set a -> a -> Bool
(∋) = flip member

-- | Element of
-- ELEMENT OF
-- Unicode: U+2208, UTF-8: E2 88 88
(∈) :: Ord a => a -> Set a -> Bool
(∈) = member

-- | Boolean and
-- LOGICAL AND
-- Unicode: U+2227, UTF-8: E2 88 A7
(∧) :: Bits a => a -> a -> a
(∧) = (.&.)

-- | Boolean or
-- LOGICAL OR
-- Unicode: U+2228, UTF-8: E2 88 A8
(∨) = (.|.)
(∨) :: Bits a => a -> a -> a

-- | Less than or equal to
-- LESS-THAN OR EQUAL TO
-- Unicode: U+2264, UTF-8: E2 89 A4
(≤) :: Ord a => a -> a -> Bool
(≤) = (<=)

-- | Greater than or equal to
-- GREATER-THAN OR EQUAL TO
-- Unicode: U+2265, UTF-8: E2 89 A5
(≥) :: Ord a => a -> a -> Bool
(≥) = (>=)

-- | Not equals
-- NOT EQUAL TO
-- Unicode: U+2260, UTF-8: E2 89 A0
(≠) :: Eq a => a -> a -> Bool
(≠) = (/=)

-- | Sequence (>>)
(➤) :: forall (m :: * -> *) a b . Monad m => m a -> m b -> m b
(➤) = (>>)

-- | Return
-- RETURN SYMBOL
-- Unicode: U+23CE, UTF-8: E2 8F 8E
(⏎) :: forall (m :: * -> *) a . Monad m => a -> m a
(⏎) = return

-- | Square root
-- SQUARE ROOT
-- Unicode: U+221A, UTF-8: E2 88 9A
(√) :: Floating a => a -> a
(√) = sqrt

