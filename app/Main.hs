module Main where

import Lib
import Code
import Control.Concurrent --(forkIO, threadDelay,writeChan,readChan,newChan)
import Control.Monad

main :: IO ()
main = do -- the arrow take an IO action and assigns the resulted action to redcode so now redcode is a string
  putStr "hi"
  redcode <- readFile "/home/damian/haskell/4thYear/redcode/src/input.txt"
  --arrow is not needed here as there is no IO action anymore
  let code = (lines redcode)
  let startPos = [0, 250, 500,750]
  let program = (bldProgram code)
  let mem = (mkMem (program,1000))
  memChan <- newChan
  pc1 <-newChan
  writeChan memChan mem
  writeChan pc1 (startPos!!0)
  t1 <- forkIO (forever $ (war True pc1 memChan))
  manage <- forkIO (forever $ (manager [(pc1,t1)] ))
  newMem <- readChan memChan
  printMem newMem 0

manager :: [(Chan Int, ThreadId)] -> IO()
manager [] = putStr ""
manager ((chan, tId):xs) = do
                        chkThread chan tId
                        manager xs

chkThread :: Chan Int -> ThreadId -> IO ()
chkThread c1 t1 = do
            pc <- readChan c1 
            if (pc == (-1))
              then killThread t1
              else writeChan c1 pc


printMem :: [Instruction] -> Int -> IO ()
printMem mem 15 = putStr ""
printMem mem i = do 
                (putStrLn (show(mem!!i)))
                printMem mem (i+1)

--wrapper function to return IO tuple
myLift :: ([Instruction],Int,Bool) -> IO ([Instruction],Int, Bool)
myLift tup = return tup

--executes instruction
war :: Bool -> Chan Int -> Chan [Instruction] -> IO ()
war alive pc c
    |alive == False = writeChan pc (-1)
    |otherwise = do
                mem <- readChan c
                crntPc <- readChan pc
                let (newMem, newPc, live) = instruct (mem!!crntPc) mem crntPc alive
                writeChan c mem 
                writeChan pc newPc

startProgram :: [Instruction] -> Int -> Bool -> IO ([Instruction], Int, Bool)
startProgram mem pc alive
                      | pc == (length mem) = (myLift (mem, 0, alive))
                      | otherwise = (startProgram newMem newPc newState)
                      where (newMem, newPc, newState) = (instruct (mem!!pc) mem pc alive)

--doInstructions :: [Instruction] -> Int -> Bool -> ([Instruction], Int, Bool)
--doInstructions mem pc alive =(doInstructions newMem newPc alive)
--              where (newMem, newPc, _) = startProgram mem pc alive
