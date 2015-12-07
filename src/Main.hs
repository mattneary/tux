module Main where

import System.Tmux
import qualified Data.Text.IO as T

main =
  do windows <- listWindows (Target "workspace")
     T.putStr windows

