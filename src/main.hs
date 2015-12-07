{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit
import Turtle

main = stdout (inproc "tmux" ["list-windows", "-t", "workspace"] "")

