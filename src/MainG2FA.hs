{- FIT VUT Brno, FLP 2019/2020 
 - Assignment: PLG-2-NKA
 - Author: Vladislav Halva
 - Login: xhalva04
 -}

module Main (main) where

import StructuresG2FA
import ParserG2FA (parseGrammar)
import ToRegularConverterG2FA (convertToRegularGrammar)
import ToAutomatonConverterG2FA (convertToFiniteAutomaton)

import System.Environment (getArgs)
import System.IO (getContents)
import Data.Maybe (isNothing, fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit (die)

-- programs input point 
-- reads arguments from command line and runs specified action on given input
main :: IO ()
main = do
    args <- getArgs
    case args of
        ([]) -> printError "Unknown arguments"
        _    -> do
                let (option:fileName) = args
                operation <- procArgs option fileName
                operation $ setStdOutIfNotDefined fileName  

-- writes hint how to run program, 
-- specified error message and terminates program
printError :: String -> IO ()
printError str = do  
    printHint 
    die str

-- processes arguments from commandline, returns operation to be executed
procArgs :: (Monad m) => String -> [String] -> m (String -> IO ())
procArgs option fileName = do
    let operation = lookup option dispatch
    if isNothing operation || length fileName > 1
        then
            return printError
        else
            return (fromJust operation)

-- maps option arguments to actions
dispatch :: [(String, String -> IO ())]
dispatch =  [ ("-i", printGrammar)
            , ("-1", printConvertedGrammar)
            , ("-2", printNFA)
            ] 

-- sets input file to special string that represents
-- reading from stdin if no filename is given
setStdOutIfNotDefined :: [String] -> String
setStdOutIfNotDefined list = if null list 
    then "#STDIN"
    else  head list

-- return contents of given file or stdin
getContentsFileOrStdin :: String -> IO String
getContentsFileOrStdin file = if file == "#STDIN"
    then do 
        input <- getContents
        return input
    else do 
        input <- readFile file 
        return input

-- prints string representation of given grammar
-- to stdout
printGrammar ::String -> IO ()
printGrammar file = do
    input <- getContentsFileOrStdin file
    putStrLn $ show $ parseGrammar input
    return ()

-- prints string representation of given grammar
-- converted to regular form to stdout
printConvertedGrammar ::String -> IO ()
printConvertedGrammar file = do
    input <- getContentsFileOrStdin file
    putStrLn $ show $ convertToRegularGrammar $ parseGrammar input
    return ()

-- prints string representation of finite automaton that
-- accepts the language specified by given the grammar
printNFA ::String -> IO ()
printNFA file = do
    input <- getContentsFileOrStdin file
    putStrLn $ show $ convertToFiniteAutomaton $ convertToRegularGrammar $ parseGrammar input
    return ()

-- prints hint how to execute this program to stdout
printHint :: IO ()
printHint = do
        putStrLn "Usage:"
        putStrLn "./plg-2-nka options [input]"
        putStrLn (  "input: is a name of input file, if not specified,"++
                    "program reads from the standard input.")
        putStrLn "options: "
        putStrLn "  -i  Prints the grammar read from input in specified format."
        putStrLn "  -1  Prints the grammar read from input converted to regular form."
        putStrLn "  -2  Prints a nondeterministic finite automaton that accepts the same language"
        putStrLn "      that is specified by the input grammar."

