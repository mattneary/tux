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

headlessSession options = newSession $ [Flag "d"] ++ options

setupServer =
  do created <- fmap isJust $  headlessSession [Parameter "c" projectDir] (Source "server")
     when created (void $ runCommand "./manage.py runserver" (Target "server"))
     Just nextWindow <- nextWorkspaceWindow
     linkWindow (windowSource "server" 0) nextWindow

setupWorkspace =
  do created <- fmap isJust $ headlessSession [Parameter "c" rootDir] (Source "workspace")
     unless created unlinkWorkspaceWindows
     setupServer

main =
  do setupWorkspace
     attachSession (Target "workspace")

