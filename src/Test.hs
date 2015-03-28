{-# LANGUAGE FlexibleInstances #-}

module Test where

import           Test.QuickCheck
import           Text.Parsec  (runParser)

import           Ticket
import           Logoot

import           Control.Monad

import Debug.Trace

testP :: (Pos, Pos) -> Bool
testP (p1, p2)
  | null (fst p1) && null (fst p2) = True
  | p1 < p2 = p1 < fst (posBetween (1,1) p1 p2)
           && fst (posBetween (1,1) p1 p2) < p2
  | p1 > p2 = p2 < fst (posBetween (1,1) p2 p1)
           && fst (posBetween (1,1) p2 p1) < p1
  | otherwise = True

genLine :: Gen [Char]
genLine = suchThat arbitrary $ \line ->
    length line > 0
 && head line /= ' '
 && head line /= '\t'
 && head line /= '\128'
 && all (/= '\n') line
 && all (/= '\r') line

instance Arbitrary (Ticket Parsed) where
  arbitrary = do
    level <- choose (1, 7)
    desc  <- genLine
    state <- elements
      [ "WORKING"
      , "WORK"
      , "W"
      , "IN-PROGRESS"
      , "TODO"
      , "DONE"
      ]
    let prefix = take level $ repeat '*'
        ticket = prefix ++ " " ++ state ++ " " ++ desc ++ "\n"
    return $ either (error . show) id $ runParser prsTicket () "" ticket

unfoldrM' :: (Monad m, MonadPlus f) => (a -> m (Maybe (b,a))) -> a -> m (f b)
unfoldrM' f z = do
        x <- f z
        case x of
                Nothing         -> return mzero
                Just (x, z)     -> do
                        xs <- unfoldrM' f z
                        return (return x `mplus` xs)

instance Arbitrary [[ParsedTicket]] where
  arbitrary = do
    old' <- arbitrary :: Gen [ParsedTicket]

    flip unfoldrM' (0, old') $ \(i, old) -> if i >= 100
      then return Nothing
      else do
        new' <- arbitrary
        return undefined

testDiffTicket :: [OrgTicket] -> [ParsedTicket] -> Bool
testDiffTicket = undefined
