module Logoot where

import Control.Arrow

import Data.Algorithm.Diff

import Data.Function
import Data.Ord
import Data.Maybe
import Data.List

import Debug.Trace

type Site = Int

type Loc = Int

type Counter = Int

type Id = (Loc, Site)

type Pos = ([Id], Counter)

type LChar = (Char, Pos)

type LString = ([LChar], [Op])

type Clock = (Site, Counter)

data Op = Ins LChar | Del LChar deriving Show

beginning :: Pos
beginning = ([(0, 0)], 0)

end :: Pos
end = ([(maxBound, 0)], 0)

incClock :: Clock -> Clock
incClock (s, h) = (s, h + 1)

clkCnt :: Clock -> Int
clkCnt = snd

emptyLString :: LString
-- emptyLString = ([('.', beginning), ('.', end)], [])
emptyLString = ([], [])

showLString :: LString -> String
showLString (cs, _) = map fst
                    $ sortBy (comparing snd)
                    $ filter (\(_, x) -> x /= beginning && x /= end) cs

posBetween :: Clock -> Pos -> Pos -> (Pos, Clock)
posBetween (s, h) (p1, h1) (p2, h2) = ((bet p1 p2, h), (s, h + 1))
  where
    bet []           p'        = [before p']
    bet p            []        = [after  p]
    bet ((pp, ps):r) ((pp', ps'):r')
      | pp == pp' && ps == ps' = (pp, ps):bet r r'
      | pp < pp' - 1           = [(rand pp pp', s)]
      | s > ps                 = [(1, s)]
      | otherwise              = [(pp, ps), after r]

    after []          = (rand 0 maxBound, s)
    after ((p, s):_)  = (rand 0 p, s)

    before []         = (rand 0 maxBound, s)
    before ((p, s):_) = (rand p maxBound, s)

    rand :: Int -> Int -> Int
    rand x y = x + 1

integrate :: [Op] -> LString -> LString
integrate ops (cs, sops) = go (ops ++ sops) (cs, [])
  where
    go [] str     = str
    go (op:ops) (cs, sops)
      | (cs', Nothing)  <- r = go ops (cs', sops)
      | (cs', Just op') <- r = go ops (cs', op' : sops)
      where
        r = go' op cs

        go' :: Op -> [LChar] -> ([LChar], Maybe Op)
        go' op@(Del c) str
          | isJust $ find (==c) str = (delete c str, Nothing)
          | otherwise               = (str, Just op)
        go' op@(Ins c@(_, (p, _))) str
          | isJust $ find ((==p) . fst . snd) str = (str, Just op)
          | otherwise                             = (c : str, Nothing)

diffLString :: Clock -> String -> LString -> ([Op], Clock)
diffLString inCl new (old, _) = go inCl diff''
  where
    go cl []                    = ([], cl)
    go cl ((_, Both c _, _):cs) = go cl cs
    go cl ((_, First c, _):cs)  = first (map Del c ++) $ go cl cs
    go cl ((l, Second c, r):cs) = first (outOps ++ ) $ go outCl cs
      where
        (outOps, (_, outCl)) = ins npos ncl' (last $ arr l) c
        ((nids, _), ncl) = posBetween cl (snd $ last $ arr l) (snd $ head $ arr r)
        ncl' = incClock ncl
        npos = (nids ++ [(0, 0)], clkCnt ncl')

        newpos :: Pos -> Clock -> (Pos, Clock)
        newpos (nids, _) cl'@(s, h) = ((init nids ++ [(h {--randomize loc--}, s)], h), incClock cl')

        ins :: Pos -> Clock -> LChar -> [LChar] -> ([Op], (Pos, Clock))
        ins pos cl' p [] = ([], (pos, cl'))
        ins pos cl' p@(_, before) ((chr, _):cs) = (Ins (chr, pos'):ops, (pos'', cl'''))
          where
            (_, after) = (head $ arr r)
            (pos', cl'') = newpos pos cl'
            (ops, (pos'', cl''')) = ins pos' cl'' (chr, pos) cs

    arr (Both c _) = c
    arr (First c)  = c
    arr (Second c) = c

    diff = getGroupedDiffBy ((==) `on` fst)
                            (sortBy (comparing snd) old)
                            (map (id &&& const beginning) new)
    diff' = Both [('.', beginning)] [('.', beginning)] : diff
         ++ [Both [('.', end)] [('.', end)]]
    diff'' = zip3 diff' (tail diff') (tail $ tail diff')

test :: String
test = showLString r
  where
    (op1, c1) = diffLString (0, 0) "adasdasd" emptyLString
    t1 = integrate op1 emptyLString
    (op1', c1') = diffLString (0, 0) "ad55dasd" t1

    (op2, c2) = diffLString (1, 0) "766" emptyLString
    t2 = integrate op2 emptyLString
    (op2', c2') = diffLString (0, 0) "cc866cc" t2

    r = integrate op2 $ integrate op1' $ integrate op1 $ integrate op2' emptyLString
