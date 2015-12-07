module Main where

import Control.Monad
import Data.Maybe
import System.Directory
import System.IO.Unsafe

import System.Tmux

-- | TODO: Just being unsafe and lazy for now!
rootDir = (unsafePerformIO getHomeDirectory) ++ "/Programs/kensho"
projectDir = rootDir ++ "/projects/zentreefish/app"

parseInt x = read x :: Int
workspaceWindows :: IO (Maybe [Int])
workspaceWindows =
  do windows <- listWindows (Target "workspace")
     return $ fmap (map (parseInt . snd . head)) windows

nextWorkspaceWindow :: IO (Maybe TmuxNoun)
nextWorkspaceWindow =
  do windows <- workspaceWindows
     return $ fmap (\ws -> windowTarget "workspace" $ (maximum ws) + 1) windows

unlinkWorkspaceWindow w = unlinkWindow (windowTarget "workspace" w)
unlinkWorkspaceWindows =
  do Just (_:windows) <- workspaceWindows
     void $ mapM unlinkWorkspaceWindow windows

setupServer =
  do created <- fmap isJust $ newSession (Source "server") [Flag "d", Parameter "c" projectDir]
     when created (void $ runCommand (Target "server") "./manage.py runserver")
     Just nextWindow <- nextWorkspaceWindow
     linkWindow (windowSource "server" 0) nextWindow

setupWorkspace =
  do created <- fmap isJust $ newSession (Source "workspace") [Flag "d", Parameter "c" rootDir]
     unless created unlinkWorkspaceWindows
     setupServer

main =
  do setupWorkspace
     attachSession (Target "workspace")

