module Main where

import Control.Monad
import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import qualified Config

import System.Tmux

parseInt x = read x :: Int

-- Workspace management actions.
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

-- Tmux configuration actions.
headlessSession options = newSession $ [Flag "d"] ++ options
setupCommand target command = runCommand (Config.command command) target
addCommandWindow rootPath session pane command =
  do let path = rootPath </> (fromMaybe "" $ Config.path command)
     splitWindow [Parameter "c" path] $ paneTarget session 0 pane
     setupCommand (paneTarget session 0 $ pane + 1) command

setupFromConfig (Config.Process rootPath cfg) =
  do let windowName = Config.name cfg
     let firstPath = rootPath </> (fromMaybe "" $ Config.rootPath cfg)
     created <- fmap isJust $
       headlessSession [Parameter "c" firstPath] (Source windowName)
     -- When initializing a process, setup each of its commands in its own split,
     -- straighten out layout, and set the window name.
     when created
          (do let (first:rest) = Config.commands cfg
              setupCommand (Target windowName) first
              mapM (uncurry $ addCommandWindow firstPath windowName) $ zip [0..] rest
              setVerticalLayout $ Target windowName
              void $ renameWindow windowName $ Target windowName)
     -- Link the window into the workspace.
     Just nextWindow <- nextWorkspaceWindow
     linkWindow (windowSource windowName 0) nextWindow

setupWorkspace rootPath cfgs =
  -- Make or reuse a workspace then link in all of the needed processes.
  do created <- fmap isJust $ headlessSession [Parameter "c" rootPath] (Source "workspace")
     unless created unlinkWorkspaceWindows
     void $ mapM setupFromConfig cfgs

getRoot [] = getCurrentDirectory
getRoot [path] = canonicalizePath path

main =
  do rootPath <- getArgs >>= getRoot
     -- Crawl the selected process tree and ensure that there are processes to be run.
     ps <- Config.getProcesses' rootPath
     when (null ps)
          (do putStrLn "Invalid config file."
              exitFailure)
     -- Run and link all processes then exit to the workspace.
     setupWorkspace rootPath ps
     attachSession (Target "workspace")

