{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Git where

import           Control.Applicative ((*>))

import           Text.Parsec
import           Text.Parsec.Char

import           System.Process

data Commit = Commit
  { cmtHash   :: String
  , cmtAuthor :: String
  , cmtDate   :: String
  } deriving Show

crlf :: (Stream s m Char) => ParsecT s u m Char
crlf                = char '\r' *> char '\n' <?> "crlf new-line"

endOfLine :: (Stream s m Char) => ParsecT s u m Char
endOfLine           = newline <|> crlf       <?> "new-line"

getCommits :: IO String
getCommits = do
  commits <- readProcess "git" ["log"] []
  return commits

parseCommit :: Stream s m Char => ParsecT s u m Commit
parseCommit = do
  let wsLine = space >> many (noneOf "\r\n") >> endOfLine

  string "commit "
  cmtHash <- many1 $ noneOf "\r\n"
  endOfLine
  string "Author: "
  cmtAuthor <- many1 $ noneOf "\r\n"
  endOfLine
  string "Date:   "
  cmtDate <- many1 $ noneOf "\r\n"
  endOfLine
  many (endOfLine <|> wsLine)

  return $ Commit {..}

parseCommits :: String -> [Commit]
parseCommits = either (const []) id . runParser (many parseCommit) () ""
