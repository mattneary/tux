module Main where

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

main = nextWorkspaceWindow >>= (putStrLn . show)

