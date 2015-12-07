{-# LANGUAGE OverloadedStrings #-}

module System.Tmux where

import Turtle
import qualified Data.Text as T
import Data.List

import System.Tmux.Parse

data TmuxObject = Target String
                | Source String
                | VarListF [String]

argList (Target n) = ["-t", T.pack n]
argList (Source n) = ["-s", T.pack n]
argList (VarListF xs) =
  let formatString = intercalate "\t" $ map (\x -> "#{" ++ x ++ "}") xs
  in ["-F", T.pack $ formatString]

tmuxCommand :: Text -> [TmuxObject] -> IO ExitCode
tmuxCommand fn objects = proc "tmux" (fn:(concatMap argList objects)) empty

listWindows :: TmuxObject -> IO ExitCode
listWindows t@(Target _) = tmuxCommand "list-windows" [t, VarListF ["window_index", "window_name"]]

