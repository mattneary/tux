{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Configuration(..)
  , Command(..)
  , ProcessEnv(..)
  , getConfig
  , getProcess
  ) where

import Data.Aeson
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

data ProcessEnv = Process { procRoot :: String, procConfig :: Configuration }

getConfig :: String -> IO (Maybe Configuration)
getConfig file =
  do exists <- doesFileExist file
     if exists then get file else fail
     where get file = do cfg <- B.readFile file
                         return $ (decode cfg :: Maybe Configuration)
           fail = return Nothing

getProcess :: String -> IO (Maybe ProcessEnv)
getProcess path =
  do config <- getConfig $ path </> ".tux.json"
     return $ Process path `fmap` config

