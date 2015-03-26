{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Git where

import           Control.Applicative ((*>), (<$>))

import qualified Data.Text           as T

import           Text.Parsec
import           Text.Parsec.Char

import           System.IO.Error
import           System.Process

data Commit = Commit
  { cmtHash   :: String
  , cmtDate   :: Int
  } deriving Show

data Content = Content
  { cntContent  :: String
  , cntCommit   :: Commit
  } deriving Show

crlf :: (Stream s m Char) => ParsecT s u m Char
crlf                = char '\r' *> char '\n' <?> "crlf new-line"

endOfLine :: (Stream s m Char) => ParsecT s u m Char
endOfLine           = newline <|> crlf       <?> "new-line"

getCommits :: FilePath -> Maybe String -> IO String
getCommits path from = do
  let args = ["log", "--date=raw", "--pretty=format:%h %ad"]
          ++ maybe [] (return . (++ "..HEAD")) from
          ++ ["-- " ++ path]
  readProcess "git" args  []

parseCommit :: Stream s m Char => ParsecT s u m Commit
parseCommit = do
  cmtHash <- many1 $ noneOf " "
  char ' '
  cmtDate <- read `fmap` (many1 $ noneOf " ")

  many1 $ noneOf "\r\n"
  (endOfLine >> return ()) <|> eof

  return $ Commit {..}

parseCommits :: String -> [Commit]
parseCommits = either (error . show) id . runParser (many parseCommit) () ""

parseContents :: String -> [Commit] -> IO [Content]
parseContents file = mapM $ \cntCommit@(Commit {..}) -> do
  cntContent <- readProcess "git" ["show", cmtHash ++ ":" ++ file] []
  return $ Content { .. }
