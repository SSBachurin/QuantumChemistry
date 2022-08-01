{-# LANGUAGE OverloadedStrings #-}

module Main where
import           CommonPhysics
import           Data.List
import           HydrogenFunctions
import           PrintToGraph
import           Scan
import           System.Exit       (exitSuccess)
import           System.IO

-- Диапазон задается (spherRange rSTP rUPB tSTP tUPB pSTP pUPB)
main :: IO ()
main = do
    putStrLn "Please, enter the command: Function - investigate atomic function, Scan - find the function borders, Print - show function's graph, Exit - close this program."
    input <- getLine
    case input of
        "Function" -> functions
        "Scan"     -> scan
        "Print"    -> printToGraph
        "Exit"     -> exitSuccess
        _          -> print  "Command not recognized"
    main







