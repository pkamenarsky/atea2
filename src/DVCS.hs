module DVCS where

import qualified Data.Text as T
import           Logoot

type FileHandle = T.Text

data File = File
  { fName     :: FileHandle
  , fParent   :: FileHandle
  , fContent  :: LString
  } deriving (Show)

data RepoOp
  = CreateFile FileHandle
  | DeleteFile FileHandle
  | UpdateFile FileHandle [Op]
  deriving (Show)

data Branch = Branch
  { brName  :: T.Text
  , brOpLog :: [RepoOp]
  } deriving (Show)

data Repo = Repo
  { rpBranches :: [Branch]
  , rpFiles    :: [File]
  } deriving (Show)
