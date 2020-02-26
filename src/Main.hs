{- FIT VUT v Brně, FLP 2019/2020 
 -Zadani projektu: PLG-2-NKA
 -Autor: Vladislav Halva
 -Login: xhalva04
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (putChar)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)

data Grammar = Grammar  { nonTerminals :: [Char] 
                        , terminals :: [Char]
                        , startNonTerminal :: Char
                        , rules :: [(Char, [Char])]
                        }

data FiniteAutomaton = FA   { states :: [Integer]
                            , startState :: Integer
                            , finishStates :: [Integer]
                            , transitions :: [(Integer, Char, Integer)]
                            }

main :: IO ()
main = do
    (option:args) <- getArgs 
    procArgs option args

-- processes arguments from commandline
procArgs :: [Char] -> [String] -> IO ()
procArgs option fileName = do
    let operation = lookup option dispatch
    if isNothing operation || length fileName > 1
        then
            putStrLn "Wrong arguments"
        else
            (fromJust operation) fileName
    return ()

-- maps option arguments to actions
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("-i", printGrammar)
            , ("-1", printConvertedGrammar)
            , ("-2", printNFA)
            ] 

printGrammar ::[String] -> IO ()
printGrammar _ = do
    putStrLn "printGrammar"
    return ()
printConvertedGrammar ::[String] -> IO ()
printConvertedGrammar _ = do
    putStrLn "convert"
    return ()
printNFA ::[String] -> IO ()
printNFA _ = do
    putStrLn "make NFA"
    return ()