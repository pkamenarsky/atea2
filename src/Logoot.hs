module Logoot where

import           Control.Arrow

import           Data.Algorithm.Diff

import           Data.Function
import           Data.Ord
import qualified Data.Map             as M
import           Data.Maybe
import           Data.List

import Debug.Trace

type Site = Int

type Loc = Int

type Counter = Int

type Clock = (Site, Counter)

type Id = (Loc, Site)

type Pos = ([Id], Counter)

type LChar = (Char, Pos, (Pos, Pos))

type LString = ([LChar], [Op], M.Map Pos Pos)
-- type LString = ([LChar], [Op])

type LFile = (LString, Clock)

data Op = Ins LChar | Del LChar deriving (Read, Show)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

beginning :: Pos
beginning = ([(0, 0)], 0)

end :: Pos
end = ([(maxBound, 0)], 0)

incClock :: Clock -> Clock
incClock (s, h) = (s, h + 1)

incSite :: Clock -> Clock
incSite (s, h) = (s + 1, h)

clkCnt :: Clock -> Int
clkCnt = snd

clkSite :: Clock -> Int
clkSite = fst

emptyLString :: LString
emptyLString = ([], [], M.empty)

emptyLFile :: Clock -> LFile
emptyLFile cl = (emptyLString, cl)

showLString :: LString -> String
showLString (cs, _, _) = map fst3
                    $ sortBy (comparing snd3)
                    $ filter (\(_, x, _) -> x /= beginning && x /= end) cs

posBetween :: Clock -> Pos -> Pos -> (Pos, Clock)
posBetween (s, h) (p1, h1) (p2, h2) = ((bet p1 p2, h), (s, h + 1))
  where
    bet []           p'        = [before p']
    bet p            []        = [after  p]
    bet ((pp, ps):r) ((pp', ps'):r')
      | pp == pp' && ps == ps' = (pp, ps):bet r r'
      | pp < pp' - 1           = [(rand pp pp', s)]
      | s > ps && s < ps'      = [(pp, s)]
      | otherwise              = [(pp, ps), after r]

    after []          = (rand 0 maxBound, s)
    after ((p, s):_)  = (rand p maxBound, s)

    before []         = (rand minBound 0, s)
    before ((p, s):_) = (rand minBound p, s)

    rand :: Int -> Int -> Int
    rand x y = x + 1

reverseIns :: Op -> Op
reverseIns (Ins c) = (Del c)
reverseIns (Del c) = (Ins c)

integrate :: [Op] -> LString -> LString
integrate ops (cs, sops, dups) = {--(\x -> trace ("INT: " ++ show ops ++ "\nSOPS" ++ show sops) x) $--} go (ops ++ sops) (cs, [], dups)
  where
    go [] str     = str
    go (op:ops) (cs, sops, dups)
      | (cs', Nothing)  <- r = go ops (cs', sops, dups)
      | (cs', Just op') <- r = go ops (cs', op' : sops, dups)
      where
        r = go' op cs

        go' :: Op -> [LChar] -> ([LChar], Maybe Op)
        go' op@(Del c) str
          | isJust $ find (==c) str = (delete c str, Nothing)
          | otherwise               = (str, Just op)
        go' op@(Ins c@(_, (p, _), _)) str
          -- | trace ("OP: " ++ show op ++ ", R: " ++ show p) False = undefined
          | isJust $ find ((==p) . fst . snd3) str = (str, Just op) --(str, Just op)
          | otherwise                             = (c : str, Nothing)

diffLString :: Clock -> String -> LString -> ([Op], Clock)
diffLString inCl new (old, _, _) = go inCl diff''
  where
    go cl []                    = ([], cl)
    go cl ((_, Both c _, _):cs) = go cl cs
    go cl ((_, First c, _):cs)  = first (map Del c ++) $ go cl cs
    go cl ((l, Second c, r):cs) = first (outOps ++ ) $ go outCl cs
      where
        (outOps, (_, outCl)) = ins (Just $ snd3 $ last $ arr l) npos ncl' c
        ((nids, _), ncl) = posBetween cl (snd3 $ last $ arr l) (snd3 $ head $ arr r)
        ncl' = incClock ncl
        npos = (nids ++ [(0, 0)], clkCnt ncl')

        newpos :: Pos -> Clock -> (Pos, Clock)
        newpos (nids, _) cl'@(s, h) = ((init nids ++ [(h {--randomize loc--}, s)], h), incClock cl')

        ins :: Maybe Pos -> Pos -> Clock -> [LChar] -> ([Op], (Pos, Clock))
        ins _ pos cl' [] = ([], (pos, cl'))
        ins mpos pos cl' ((chr, _, _):cs) = (Ins (chr, pos', (cpos, after)):ops, (pos'', cl'''))
          where
            cpos = fromMaybe pos mpos
            (_, after, _) = (head $ arr r)
            (pos', cl'') = newpos pos cl'
            (ops, (pos'', cl''')) = ins Nothing pos' cl'' cs

    arr (Both c _) = c
    arr (First c)  = c
    arr (Second c) = c

    diff = getGroupedDiffBy ((==) `on` fst3)
                            (sortBy (comparing snd3) old)
                            (map (\x -> (x, beginning, undefined)) new)
    diff' = Both [('.', beginning, (beginning, end))] [('.', beginning, (beginning, end))] : diff
         ++ [Both [('.', end, (beginning, end))] [('.', end, (beginning, end))]]
    diff'' = zip3 diff' (tail diff') (tail $ tail diff')

test :: String
test = show r
  where
    (op1, c1) = diffLString (0, 0) "adasdasd" emptyLString
    t1 = integrate op1 emptyLString
    (op1', c1') = diffLString (0, 0) "ad55dasd" t1

    (op2, c2) = diffLString (1, 0) "766" emptyLString
    t2 = integrate op2 emptyLString
    (op2', c2') = diffLString (0, 0) "cc866cc" t2

    -- r = integrate op1 $ integrate op1' $ integrate op2 $ integrate op2' emptyLString
    r = integrate op1 emptyLString
