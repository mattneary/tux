module Main where

import System.Tmux

parseInt x = read x :: Int

workspaceWindows :: IO [Int]
workspaceWindows =
  do windows <- listWindows (Target "workspace")
     return $ map (parseInt . snd . head) windows

nextWorkspaceWindow :: IO Int
nextWorkspaceWindow =
  do windows <- workspaceWindows
     return $ (maximum windows) + 1

main = nextWorkspaceWindow >>= (putStrLn . show)

