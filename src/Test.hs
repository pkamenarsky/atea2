module Test where

import Test.QuickCheck
import Logoot

testP :: (Pos, Pos) -> Bool
testP (p1, p2)
  | null (fst p1) && null (fst p2) = True
  | p1 < p2 = p1 < fst (posBetween (1,1) p1 p2)
           && fst (posBetween (1,1) p1 p2) < p2
  | p1 > p2 = p2 < fst (posBetween (1,1) p2 p1)
           && fst (posBetween (1,1) p2 p1) < p1
  | otherwise = True
