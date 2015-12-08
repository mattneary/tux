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

setupCommand target command = runCommand (Config.command command) target
setupFromConfig rootPath cfg =
  do let windowName = Config.name cfg
     created <- fmap isJust $
       headlessSession
         [Parameter "c" (rootPath </> (fromMaybe "" $ Config.rootPath cfg))]
         (Source windowName)
     when created (void $ mapM (setupCommand $ Target windowName) (Config.commands cfg))
     Just nextWindow <- nextWorkspaceWindow
     linkWindow (windowSource windowName 0) nextWindow

setupWorkspace rootPath cfgs =
  do created <- fmap isJust $ headlessSession [Parameter "c" rootPath] (Source "workspace")
     unless created unlinkWorkspaceWindows
     void $ mapM (setupFromConfig rootPath) cfgs

getConfigFile :: [String] -> IO (Maybe (String, Config.Configuration))
getConfigFile [] =
  do cwd <- getCurrentDirectory
     fmap (fmap $ (,) cwd) $ Config.getConfig $ cwd </> ".tux.json"
getConfigFile [file] =
  do path <- canonicalizePath $ file
     fmap (fmap $ (,) path) $ Config.getConfig $ path </> ".tux.json"

main =
  do cfg <- getArgs >>= getConfigFile
     when (isNothing cfg)
          (do putStrLn "Invalid config file."
              exitFailure)
     let Just (root, c) = cfg
     setupWorkspace root [c]
     attachSession (Target "workspace")

