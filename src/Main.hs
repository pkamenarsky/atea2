module Main where

import           System.Environment
import qualified System.IO.Strict as SIO

import           Logoot
import           Woot


--

writeWFile :: FilePath -> WFile -> IO ()
writeWFile fp = writeFile fp . show

readWFile :: FilePath -> IO WFile
readWFile fp = read `fmap` SIO.readFile fp

main :: IO ()
main = do
  args <- getArgs

  case args of
    "--create":fp:_ -> do
      writeFile (fp ++ ".txt") "-- Created by atea"
      writeWFile (fp ++ ".atea") createWFile
    "--push":fp:rmt:_ -> push fp rmt
    "--pull":fp:rmt:_ -> do
      push fp rmt

  print ""
    where
      push fp rmt = do
        tf   <- SIO.readFile (fp ++ ".txt")
        wf   <- readWFile (fp ++ ".atea")
        -- rmtf <- readWFile (rmt ++ ".atea")

        let (wf', ops) = diffWFile wf tf

        writeWFile (fp ++ ".atea") wf'
        writeFile (fp ++ ".op") $ show ops
        -- writeWFile (rmt ++ ".atea") $ mergeWFile ops rmtf

main2 :: IO ()
main2 = do
  print $ showWFileString f1'''
  print $ showWFileString f2'''
  where
    f1 = createWFile
    f2 = cloneWFile f1

    (f1', o1) = diffWFile f1 "1234567890"
    (f1'', o1') = diffWFile f1' "oo1a23hhh890lalal"
    (f2', o2) = diffWFile f2 "abcdefghij"
    (f2'', o2') = diffWFile f2' "abefghij888"

    f1''' = mergeWFile o2' $ mergeWFile o1' $ mergeWFile o2 f1'
    f2''' = mergeWFile o1' $ mergeWFile o1 $ mergeWFile o2' f2'
