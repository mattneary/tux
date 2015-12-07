module Main where

import System.Tmux

main = listWindows (Target "workspace")

