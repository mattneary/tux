{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module System.Tmux
  ( TmuxNoun(..)
  , listWindows
  ) where

import Turtle
import qualified Data.Text as T
import Data.List

import System.Tmux.Parse

-- | A TmuxObject can be reduced to an argList to be passed to tmux.
class TmuxObject t where
  argList :: t -> [Text]

-- | A TmuxArg wraps a TmuxObject, hiding which sort it is.
data TmuxArg where
  MkArg :: (TmuxObject t) => t -> TmuxArg
getArgs (MkArg a) = argList a

-- | Allow tmux entities to be referenced by name.
data TmuxNoun = Target String | Source String
instance TmuxObject TmuxNoun where
  argList (Target n) = ["-t", T.pack n]
  argList (Source n) = ["-s", T.pack n]

-- | Allow tmux format strings to be constructed.
data TmuxFormat = VarList [String]
instance TmuxObject TmuxFormat where
  argList (VarList xs) = ["-F", T.pack $ formatString]
    where formatString = intercalate "\t" $ map (\x -> "#{" ++ x ++ "}") xs

tmuxCommand :: Text -> [TmuxArg] -> IO (Maybe Text)
tmuxCommand fn objects =
  do (e, t) <- procStrict "tmux" (fn:concatMap getArgs objects) empty
     return (if e == ExitSuccess
             then Just t
             else Nothing)

-- | A wrapper of `tmux list-windows -t XXX`.
listWindows :: TmuxNoun -> IO (Maybe [[(String, String)]])
listWindows t@(Target _) =
  do let vars = ["window_index", "window_name"]
     out <- tmuxCommand "list-windows" [MkArg t, MkArg $ VarList vars]
     return $
       fmap (\t -> let Right parsedVars = parseOutput vars (T.unpack t)
                   in parsedVars) out

