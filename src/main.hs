{-# LANGUAGE OverloadedStrings #-}

module Main where

import Turtle

main = stdout (inproc "tmux" ["list-windows", "-t", "workspace"] "")

