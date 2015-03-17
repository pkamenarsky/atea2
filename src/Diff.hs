{-# LANGUAGE TupleSections #-}

module Diff where

import           Control.Arrow

import           Data.Algorithm.Diff
import           Data.Function

import           Data.List

diff :: ([a] -> Int) -> (a -> a -> Bool) -> [[a]] -> [[a]] -> [Diff a]
diff hash cmp old new = go hashdiff
  where
    words :: [a] -> [[a]]
    words = undefined

    hashOld   = map (id &&& hash) old
    hashNew   = map (id &&& hash) new
    hashdiff  = getGroupedDiffBy ((==) `on` snd) hashOld hashNew

    go [] = []
    go (First c:Second c':cs) = linediff c c' ++ go cs
    go (Second c:First c':cs) = linediff c' c ++ go cs
    go (Both c c':cs) = concat (zipWith (\x y -> zipWith Both (fst x) (fst y)) c c') ++ go cs
    go (First c:cs) = concatMap (map First . fst) c ++ go cs
    go (Second c:cs) = concatMap (map Second . fst) c ++ go cs

    linediff ls [] = concatMap (map First . fst) ls
    linediff [] rs = concatMap (map Second . fst) rs
    linediff lold@(l:ls) lnew@(l':ls') = getDiffBy cmp (fst l) (fst l') ++ linediff ls ls'
      where
        -- TODO: matach old and new lines by shortest edit distance
        -- if edit distance > 75%, genereate deletes
        -- if no matching old line, generate inserts
        worddiff =
          [ diff hash cmp (words lnOld) (words lnNew)
          | [lnNew, lnOld] <- sequence [map fst ls', map fst ls]
          ]

data BDiff a = Ins a | Copy [a] deriving Show

bdiff :: Eq a => [a] -> [a] -> [BDiff a]
bdiff _ []           = []
bdiff old new@(x:xs) | null pr = Ins x : bdiff old xs
                     | otherwise = Copy pr : bdiff old (drop (length pr) new)
  where
    pr = maxPrefix new old
    maxPrefix [] _                      = []
    maxPrefix pr str | isInfixOf pr str = pr
                     | otherwise        = maxPrefix (init pr) str

