{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Configuration(..)
  , Command(..)
  , ProcessEnv(..)
  , getConfig
  , getProcesses
  , getProcesses'
  ) where

import Data.Aeson
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics
import System.Directory
import System.FilePath.Posix

data Command = Command {
  path :: Maybe String,
  command :: String
} deriving (Generic, Show)
instance FromJSON Command

data Configuration = Configuration {
  name :: String,
  commands :: [Command],
  children :: Maybe [String],
  rootPath :: Maybe String
} deriving (Generic, Show)
instance FromJSON Configuration

getChildren = concat . maybeToList . children

data ProcessEnv = Process { procRoot :: String, procConfig :: Configuration }

instance Eq ProcessEnv where
  Process _ (Configuration m _ _ _) == Process _ (Configuration n _ _ _) =
    m == n

getConfig :: String -> IO (Maybe Configuration)
getConfig file =
  -- Return the specified config file if it exists and parses.
  do exists <- doesFileExist file
     if exists then get file else fail
     where get file = do cfg <- B.readFile file
                         return $ (decode cfg :: Maybe Configuration)
           fail = return Nothing

getProcess :: String -> IO (Maybe ProcessEnv)
getProcess path =
  do config <- getConfig $ path </> ".tux.json"
     return $ Process path `fmap` config

getProcesses :: String -> IO [ProcessEnv]
getProcesses path =
  -- Parse the root process, crawl all of its children, and return the whole tree.
  do roots <- fmap maybeToList $ getProcess path
     let children = map (path </>) $ concatMap (getChildren . procConfig) roots
     ancestors <- fmap concat . sequence $ map getProcesses children
     return $ roots ++ ancestors

getProcesses' = fmap nub . getProcesses

