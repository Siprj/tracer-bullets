module Main where

import Control.Monad
import System.IO

import Lib
import ObjLoader

main :: IO ()
main = someFunc
-- main = loadModel "pokus.obj" >>= print
