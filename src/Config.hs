{-# LANGUAGE DeriveGeneric #-}

module Config
  ( Configuration(..)
  , Command(..)
  , getConfig
  ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import GHC.Generics

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

getConfig :: String -> IO (Maybe Configuration)
getConfig file =
  do cfg <- B.readFile file
     return $ (decode cfg :: Maybe Configuration)

