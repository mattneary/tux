module Main where

import Control.Monad
import Data.Maybe
import System.Tmux

parseInt x = read x :: Int
workspaceWindows :: IO (Maybe [Int])
workspaceWindows =
  do windows <- listWindows (Target "workspace")
     return $ fmap (map (parseInt . snd . head)) windows

nextWorkspaceWindow :: IO (Maybe Int)
nextWorkspaceWindow =
  do windows <- workspaceWindows
     return $ fmap ((+1) . maximum) windows

unlinkWorkspaceWindow w = unlinkWindow (windowTarget "workspace" w)
unlinkWorkspaceWindows =
  do Just windows <- workspaceWindows
     void $ mapM unlinkWorkspaceWindow windows

setupWorkspace =
  do created <- fmap isJust $ newSession True (Source "workspace")
     unless created unlinkWorkspaceWindows

main =
  do setupWorkspace
     attachSession (Target "workspace")

