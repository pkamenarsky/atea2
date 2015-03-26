{-# LANGUAGE FlexibleContexts, RecordWildCards #-}

module Git where

import           Control.Applicative ((*>), (<$>))

import qualified Data.Text           as T

import           Text.Parsec
import           Text.Parsec.Char

import           System.IO.Error
import           System.Process

import           GHC.IO.Handle

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

readProcessCwd :: FilePath -> String -> [String] -> IO String
readProcessCwd path cmd args = do
  (_, Just hout, _, _) <-
    createProcess (proc cmd args){ cwd = Just path
                                 , std_out = CreatePipe }
  hGetContents hout

getCommits :: FilePath -> Maybe String -> IO String
getCommits path from = do
  let args = ["log", "--date=raw", "--pretty=format:%h %ad"]
          ++ maybe [] (return . (++ "..HEAD")) from
  readProcessCwd path "git" args

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

parseContents :: FilePath -> String -> [Commit] -> IO [Content]
parseContents path file = mapM $ \cntCommit@(Commit {..}) -> do
  cntContent <- readProcessCwd path "git" ["show", cmtHash ++ ":" ++ file]
  return $ Content { .. }
