{-# LANGUAGE RecordWildCards #-}

module Woot where

import Control.Arrow
import Control.Monad.State

import Data.Algorithm.Diff
import Data.Function
import Data.Maybe
import Data.List

import Debug.Trace

data CharId = Beginning | End | CharId Int Int deriving (Show, Read, Eq, Ord)

incCid :: CharId -> CharId
incCid (CharId x y) = CharId x (y + 1)

cloneCid :: CharId -> CharId
cloneCid (CharId x _) = CharId (x + 1) 0

data Op = Insert [(Char, CharId)] CharId CharId | Delete [CharId] deriving (Show, Read, Eq, Ord)

data WFile = WFile
  { wfNext  :: CharId
  , wfSting :: WString
  } deriving (Show, Read)

data WChar = WChar
  { wcId :: CharId
  , wcChar :: Char
  , wcPrev :: CharId
  , wcNext :: CharId
  , wcVisible :: Bool
  } deriving (Show, Read)

data WString = WString
  { wsChars :: [WChar]
  , wsOps   :: [Op]
  } deriving (Show, Read)

showWString :: WString -> String
showWString = map wcChar . filter wcVisible . wsChars
-- showWString = map wcChar

indexOf :: CharId -> WString -> Int
indexOf Beginning _ = 0
indexOf End str = length (wsChars str)
indexOf cid str = fromMaybe (error "indexOf") $ findIndex ((== cid) . wcId) (wsChars str)

subseq :: CharId -> CharId -> WString -> WString
subseq prev next str = str { wsChars = take (to - from) $ drop from $ wsChars str }
  where
    from | prev == Beginning = 0
         | otherwise         = indexOf prev str + 1
    to   | next == End       = length $ wsChars str
         | otherwise         = indexOf next str


ins :: WString -> WChar -> Int -> WString
ins str c pos = str { wsChars = take pos chars ++ [c] ++ drop pos chars }
  where
    chars = wsChars str

hide :: WString -> CharId -> WString
hide str cid = str { wsChars = take pos chars ++ [c { wcVisible = False } ] ++ drop (pos + 1) chars }
  where
    pos = indexOf cid str
    c = wsChars str !! pos
    chars = wsChars str

trim :: WString -> WString
trim str = str { wsChars = [ s | s <- wsChars str, all (\x -> wcId x /= wcNext s && wcId x /= wcPrev s) $ wsChars str ] }

canIntegrateOp :: WString -> Op -> Bool
canIntegrateOp str (Insert _ prev next) = canIntegrate' prev && canIntegrate' next
  where
    canIntegrate' cid = cid == Beginning || cid == End || any ((cid ==) . wcId) (wsChars str)
canIntegrateOp str (Delete cids) = all (\cid -> any ((cid==) . wcId) $ wsChars str) cids

integrate :: WChar -> CharId -> CharId -> WString -> WString
integrate c before after str
  -- | trace (showWString str) False = undefined
  | any (wcId c==) $ map wcId $ wsChars str = str
  | null $ wsChars ss   = ins str c (indexOf after str)
  | otherwise           = integrate c before' after' str
  where ss = subseq before after str
        l  = before : (map wcId $ wsChars $ trim ss) ++ [after]
        l' = zip l $ tail l
        (before', after') = case dropWhile ((wcId c <) . snd) l' of
          []     -> last l'
          (x:_)  -> x

integrateOp :: Op -> WString -> WString
integrateOp (Insert c prev next) str
  = foldl (\str' (c', cid, prev') -> integrate (WChar cid c' prev' next True) prev' next str')
                str $ zip3 (map fst c) (map snd c) (prev : map snd c)
integrateOp (Delete cids) str = foldl hide str cids

integrateOps :: [Op] -> WString -> WString
integrateOps ops str = integrateOps' $ str { wsOps = ops ++ wsOps str }
  where
    integrateOps' str
      | Just op <- find (canIntegrateOp str) (wsOps str)
        = integrateOps' $ integrateOp op $ str { wsOps = delete op $ wsOps str }
      | otherwise = str

--

diffWString :: CharId -> WString -> String -> [Op]
diffWString cid old new = go cid diff''
  where
    go cid []                    = []
    go cid ((_, Both c _, _):cs) = go cid cs
    go cid ((_, First c, _):cs)  = Delete (map snd c):go cid cs
    go cid ((l, Second c, r):cs) = Insert (zip (map fst c) (take (length c) cids))
                                          (snd $ last $ arr l)
                                          (snd $ head $ arr r)
                                          : go (cids !! length c) cs
      where
        cids = iterate incCid cid

    arr (Both c _) = c
    arr (First c)  = c
    arr (Second c) = c

    diff = getGroupedDiffBy ((==) `on` fst)
                            (map (wcChar &&& wcId) $ wsChars old)
                            (map (id &&& const Beginning) new)
    diff' = Both [('.', Beginning)] [('.', Beginning)] : diff
         ++ [Both [('.', End)] [('.', End)]]
    diff'' = zip3 diff' (tail diff') (tail $ tail diff')

--

createWFile :: WFile
createWFile = WFile (CharId 0 0) (WString [] [])

cloneWFile :: WFile -> WFile
cloneWFile (WFile {..}) = WFile (cloneCid wfNext) wfSting

diffWFile :: WFile -> String -> (WFile, [Op])
diffWFile (WFile {..}) str = (WFile (incCid $ maximum $ map opCid ops) (integrateOps ops wfSting), ops)
  where
    ops = diffWString wfNext wfSting str

    opCid (Insert cs _ _) = maximum $ map snd cs
    opCid _               = CharId 0 0

mergeWFile :: [Op] -> WFile -> WFile
mergeWFile ops (WFile {..}) = WFile wfNext (integrateOps ops wfSting)

showWFileString :: WFile -> String
showWFileString = showWString . wfSting

