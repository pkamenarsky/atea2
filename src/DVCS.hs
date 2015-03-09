module DVCS where

import qualified Data.Text as T
import           Logoot

type FileHandle = T.Text

data File = File
  { fName     :: FileHandle
  , fParent   :: FileHandle
  , fContent  :: LString
  } deriving (Show)

data Repo = Repo
  { rpOpLog :: [(FileHandle, [Op])]
  , rpFiles :: [File]
  } deriving (Show)
