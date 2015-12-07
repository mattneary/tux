{-# LANGUAGE OverloadedStrings #-}

module System.Tmux where

import Turtle
import qualified Data.Text as T

class TmuxObject t where
  argList :: t -> [Text]

data TmuxNoun = Target String | Source String
instance TmuxObject TmuxNoun where
  argList (Target n) = ["-t", T.pack n]
  argList (Source n) = ["-s", T.pack n]

tmuxCommand fn args = proc "tmux" (fn:args) empty

listWindows :: TmuxNoun -> IO ExitCode
listWindows t@(Target _) = tmuxCommand "list-windows" (argList t)

