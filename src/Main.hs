module Main where

import System.Tmux

main =
  do windows <- listWindows (Target "workspace")
     putStrLn $ show windows

