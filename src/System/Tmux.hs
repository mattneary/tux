{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module System.Tmux
  ( TmuxNoun(..)
  , TmuxOption(..)
  , windowTarget
  , windowSource
  , listWindows
  , newSession
  , attachSession
  , unlinkWindow
  , linkWindow
  , runCommand
  ) where

import Turtle
import qualified Data.Text as T
import Data.List

import System.Tmux.Parse
import System.Posix.Process as P

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

windowNoun :: (String, Int) -> String
windowNoun (s, w) = s ++ ":" ++ (show w)
windowTarget = curry $ Target . windowNoun
windowSource = curry $ Source . windowNoun

-- | Allow tmux format strings to be constructed.
data TmuxFormat = VarList [String]
instance TmuxObject TmuxFormat where
  argList (VarList xs) = ["-F", T.pack formatString]
    where formatString = intercalate "\t" $ map (\x -> "#{" ++ x ++ "}") xs

-- | Allow additional tmux parameters to be passed.
data TmuxOption = Flag String | Parameter String String | Argument String
instance TmuxObject TmuxOption where
  argList (Flag x) = [T.pack $ "-" ++ x]
  argList (Parameter k v) = map T.pack ["-" ++ k, v]
  argList (Argument x) = [T.pack x]

tmuxStart :: String -> [TmuxArg] -> IO ()
tmuxStart fn objects =
  do P.executeFile "tmux" True (fn:concatMap (map T.unpack . getArgs) objects) Nothing
     return ()

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

newSession s@(Source _) rest =
  tmuxCommand "new-session" $ map MkArg rest ++ [MkArg s]
attachSession t@(Target _) = tmuxStart "attach-session" [MkArg t]
unlinkWindow t@(Target _) = tmuxCommand "unlink-window" [MkArg t]
linkWindow s@(Source _) t@(Target _) = tmuxCommand "link-window" [MkArg s, MkArg t]
runCommand t@(Target _) command =
  tmuxCommand "send-keys" $ [MkArg t] ++ map (MkArg . Argument) [command, "Enter"]

