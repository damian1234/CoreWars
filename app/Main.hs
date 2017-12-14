module Main where

import Lib
import Code

main :: IO ()
main = do -- the arrow take an IO action and assigns the resulted action to redcode so now redcode is a string
  redcode <- readFile "/home/damian/haskell/4thYear/redcode/src/input.txt"
  --arrow is not needed here as there is no IO action anymore
  let code = (lines redcode)
  let program = (bldProgram code)
  let mem = (mkMem (program,80))
  (putStr (show(mem!!55)))
  

