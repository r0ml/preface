{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- This is an implementation of the O(ND) diff algorithm as described here
-- <http://xmailserver.org/diff2.pdf>.
-- The algorithm is the same one used by standared Unix diff.
-----------------------------------------------------------------------------

module Preface.Diff (
  diff, DiffOperation(..)
) where

import Preface.Imports
import Preface.Console
import Preface.Stringy

-- import qualified Data.Array.IArray as A ((!))

data WhichInput = First | Second | Both deriving (Show, Eq)

data SingleDiff a = SingleLeft a | SingleRight a | SingleBoth a a deriving (Show, Eq)
data MultiDiff a = MultiLeft [a] | MultiRight [a] | MultiBoth [a] [a] deriving (Show, Eq)

data DL = DL {poi :: !Int, poj :: !Int, path::[WhichInput]} deriving (Show, Eq)

instance Ord DL where x <= y = if poi x == poi y then  poj x > poj y else poi x <= poi y

canDiag :: forall a . (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> Int -> Int -> Int -> Int -> Bool
canDiag eq as bs lena lenb = 
    let arAs = fromList as :: Seq a 
        arBs = fromList bs :: Seq a
     in  \ i j -> (( i < lena && j < lenb ) && ((arAs ! i) `eq` (arBs ! j)))

dstep :: (Int -> Int -> Bool) -> [DL] -> [DL]
dstep cd dls = f1 (head dls) : nextstep dls
    where f1 dl = addsnake cd $ dl {poi=poi dl + 1, path=First : path dl}
          f2 dl = addsnake cd $ dl {poj=poj dl + 1, path=Second : path dl}
          nextstep (hd:tl) = let dl2 = f2 hd in if null tl then [dl2] else max dl2 (f1 $ head tl) : nextstep tl
          nextstep [] = error "nextstep [] unimplemented"

addsnake :: (Int -> Int -> Bool) -> DL -> DL
addsnake cd dl = if cd ppi pj then addsnake cd $ dl {poi = ppi + 1, poj = pj + 1, path=Both : path dl} else dl
    where ppi = poi dl; pj = poj dl

-- | Longest Common Subsequence (and Shortest Edit Sequence )
lcs :: (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> [WhichInput]
lcs eq as bs = path . head . dropWhile (\dl -> poi dl /= lena || poj dl /= lenb) .
               concat . iterate (dstep cd) . (:[]) . addsnake cd $ DL {poi=0,poj=0,path=[]}
            where cd = canDiag eq as bs lena lenb
                  lena = length as; lenb = length bs

getDiffBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> [SingleDiff a]
getDiffBy eq a b = markup a b . reverse $ lcs eq a b
    where markup (x:xs)   ys (First:ds) = SingleLeft x : markup xs ys ds
          markup   xs   (y:ys) (Second:ds) = SingleRight y : markup xs ys ds
          markup (x:xs) (y:ys) (Both:ds) = SingleBoth x y : markup xs ys ds
          markup _ _ _ = []

groupDiffs :: Eq a => [SingleDiff a] -> [MultiDiff a]
groupDiffs = doit
    where doit (SingleLeft x : xs) = MultiLeft (x:fs) : doit rest where (fs, rest) = getLefts xs
          doit (SingleRight x : xs) = MultiRight(x:fs) : doit rest where (fs, rest) = getRights xs
          doit (SingleBoth x y : xs) = MultiBoth (x:fsx) (y:fsy) : doit rest where (fsx, fsy, rest) = getBoths xs
          doit [] = []
          getLefts (SingleLeft x : xs) = (x:fs, rest) where (fs, rest) = getLefts xs
          getLefts ls = ([], ls)
          getRights (SingleRight x : xs) = (x:fs, rest) where (fs, rest) = getRights xs
          getRights rs = ([], rs)
          getBoths (SingleBoth x y : xs) = (x:fsx, y:fsy, rest) where (fsx, fsy, rest) = getBoths xs
          getBoths bs = ([],[],bs)

diff :: (Eq a ) => [a] -> [a] -> [DiffOperation a]
diff x y = coalesce $ toDiffOp $ groupDiffs $ getDiffBy (==) x y

toDiffOp :: Eq a => [MultiDiff a] -> [DiffOperation a]
toDiffOp = toLineRange 1 1
    where
       toLineRange _ _ []=[]
       toLineRange ls rs (MultiBoth lc rc:xtail)= Unchanged ls lc rs rc : toLineRange (ls+length lc) (rs+length rc) xtail
       toLineRange ls rs (MultiRight rc: MultiLeft lc:rest)= toChange ls rs lc rc rest
       toLineRange ls rs (MultiLeft lc: MultiRight rc:rest)= toChange ls rs lc rc rest
       toLineRange ls rs (MultiRight rc:rest)= Addition rs rc ls : toLineRange ls (rs + length rc) rest
       toLineRange ls rs (MultiLeft lc:rest) = Deletion ls lc rs : toLineRange (ls + length lc) rs rest
       toChange ls rs lc rc rest = Change ls lc rs rc : toLineRange (ls+length lc) (rs+length rc) rest

coalesce :: Eq a => [DiffOperation a] -> [DiffOperation a]
coalesce x = reverse . snd $ coalzip (x,[])

backspace :: ([a], [a]) -> ([a], [a])
backspace (a, b:c) = (b : a, c)
backspace (a, []) = (a,[])

xchng :: Eq a =>  LineNo -> [a] -> LineNo -> [a] -> [DiffOperation a]
xchng ls lc rs rc =
  let rl = length rc
      ll = length lc
  in if rc `isSuffixOf` lc then [Deletion ls (take (length lc - rl) lc) rs 
            , Unchanged (ls+ll-rl) (drop (ll - rl) lc) rs rc]
     else [Change ls lc rs rc]

coalzip :: Eq a => ([DiffOperation a],[DiffOperation a])-> ([DiffOperation a],[DiffOperation a])
-- if I stick a "crunch" in on this Change creation, it fails to do the right thing?
coalzip (a@(Deletion ls1 lc1 _rs1) : b@(Unchanged _ls2 lc2 rs2 rc2) : c@(Deletion _ls3 lc3 _rs3) : rest, z) =
  let ch = xchng ls1 (lc1++lc2++lc3) rs2 rc2 
  in coalzip (if length lc2 < 3 then backspace ( ch ++ rest, z) else (b : c : rest, a : z))
coalzip (a@(Change ls1 lc1 rs1 rc1) : b@(Unchanged _ls2 lc2 _rs2 rc2) : c@(Deletion _ls3 lc3 _rs3) : rest, z) =
  let ch = xchng ls1 (lc1++lc2++lc3) rs1 (rc1++rc2) 
  in coalzip (if length lc2 < 3 then backspace (ch ++ rest, z) else (b : c : rest, a : z))
coalzip (a@(Deletion ls1 lc1 _rs1) : b@(Unchanged _ls2 lc2 rs2 rc2) : c@(Change _ls3 lc3 _rs3 rc3) : rest, z) =
  let ch = xchng ls1 (lc1++lc2++lc3) rs2 (rc2++rc3) 
  in coalzip (if length lc2 < 3 then backspace (ch ++ rest, z) else (b : c : rest, a : z))
coalzip (a@(Change ls1 lc1 rs1 rc1) : b@(Unchanged _ls2 lc2 _rs2 rc2) : c@(Change _ls3 lc3 _rs3 rc3) : rest, z) =
  let ch = xchng ls1 (lc1++lc2++lc3) rs1 (rc1++rc2++rc3) 
  in coalzip (if length lc2 < 3 then backspace (ch ++ rest, z) else (b : c : rest, a : z))
coalzip (a@(Addition rs1 rc1 _ls1) : b@(Unchanged ls2 lc2 _rs2 rc2) : c@(Addition _rs3 rc3 _ls3) : rest, z) =
  let ch = xchng ls2 lc2 rs1 (rc1++rc2++rc3) 
  in coalzip (if length rc2 < 3 then backspace ( ch ++ rest, z) else (b : c : rest, a : z))
coalzip (a@(Change ls1 lc1 rs1 rc1) : b@(Unchanged _ls2 lc2 _rs2 rc2) : c@(Addition _rs3 rc3 _ls3) : rest, z) =
  let ch = xchng ls1 (lc1++lc2) rs1 (rc1++rc2++rc3)
  in coalzip (if length rc2 < 3 then backspace (ch ++ rest, z) else (b : c : rest, a : z))
coalzip (a@(Addition rs1 rc1 _ls1) : b@(Unchanged ls2 lc2 _rs2 rc2) : c@(Change _ls3 lc3 _rs3 rc3) : rest, z) =
  let ch = xchng ls2 (lc2++lc3) rs1 (rc1++rc2++rc3)
  in coalzip (if length rc2 < 3 then backspace (ch ++ rest, z) else (b : c : rest, a : z))
coalzip ((Unchanged ls1 lc1 rs1 rc1) : (Unchanged _ls2 lc2 _rs2 rc2) : rest, z) =
  coalzip $ backspace $ backspace ( Unchanged ls1 (lc1++lc2) rs1 (rc1++rc2) : rest, z)
coalzip (a:b,z) = coalzip (b, a : z)
coalzip ([],z) = ([],z)

prettyLines :: Char -> [Text] -> Text
prettyLines start lins = strConcat $ map (\x -> strConcat [ strCons start " ",x,"\n"]) lins

type LineNo = Int


data LineRange = LineRange Int Int
instance Show LineRange where
  show (LineRange start len) = if len == 1 then show start else concat [show start,"," ,show (start + len - 1)]

-- data LineRange a = LineRange { lrNumbers :: LineNoPair, lrContents :: [a] } deriving (Show)

data DiffOperation a = Deletion LineNo [a] LineNo 
  | Addition LineNo [a] LineNo 
  | Change LineNo [a] LineNo [a]
  | Unchanged LineNo [a] LineNo [a]

instance Show (DiffOperation Text) where
  show (Deletion ls lc rs) =
   concat [show (LineRange ls (length lc)), "d", show rs,"\n", asString $ prettyLines '<' lc]
  show (Addition rs rc ls) =
    concat[ show ls, "a" , show (LineRange rs (length rc)), "\n", asString $ prettyLines '>' rc]
  show (Change ls lc rs rc) =
    concat [ show (LineRange ls (length lc) ), "c" , show (LineRange rs (length rc)), "\n",
       asString $ prettyLines '<' lc, "---\n", asString $ prettyLines '>' rc ]
  show (Unchanged _ls _lc _rs _rc) = ""
--      concat [ show (lrNumbers inLeft), "c" , show (lrNumbers inRight), "\n",
--         prettyLines '<' (lrContents inLeft), "---\n", prettyLines '>' (lrContents inRight) ]

bgColor :: Int
bgColor = 15 

instance Show (DiffOperation Char) where
  show (Deletion _ls lc _rs) = concat [asString consolePeach, asString $ consoleSetExtendedBackgroundColor bgColor, lc, asString treset :: String] 
  show (Addition _rs rc _ls) = asString $ strConcat[ asString consoleAzure, asString $ consoleSetExtendedBackgroundColor bgColor, rc, asString treset]
  show (Change _ls lc _rs rc) =
    asString $ strConcat [ asString $ consoleSetExtendedBackgroundColor bgColor, asString $  consoleSetColor ConsoleAttrDullBlack, "{", asString consolePeach, lc, asString consoleAzure, asString rc, asString $ consoleSetColor ConsoleAttrDullBlack, "}", asString treset ]
  show (Unchanged _ls lc _rs _rc) = lc

