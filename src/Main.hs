{- FIT VUT v Brně, FLP 2019/2020 
 -Zadani projektu: PLG-2-NKA
 -Autor: Vladislav Halva
 -Login: xhalva04
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (putChar, IOMode(ReadMode), withFile, hGetContents)
import Data.Maybe (isNothing, isJust, fromJust, fromMaybe)
import Data.List.Split (splitOn)
import Data.Char (isUpper, isLower)

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
    (option:fileName) <- getArgs 
    operation <- procArgs option fileName
    operation $ setStdOutIfNotDefined fileName
    return ()


-- processes arguments from commandline, return operation to be executed
procArgs :: (Monad m) => [Char] -> [String] -> m ([String] -> IO ())
procArgs option fileName = do
    let operation = lookup option dispatch
    if isNothing operation || length fileName > 1
        then
            return printError
        else
            return (fromJust operation)

-- maps option arguments to actions
dispatch :: [(String, [String] -> IO ())]
dispatch =  [ ("-i", printGrammar)
            , ("-1", printConvertedGrammar)
            , ("-2", printNFA)
            ] 

setStdOutIfNotDefined :: [String] -> [String]
setStdOutIfNotDefined list = if length list == 0 
    then ["#STDOUT"]
    else list

printGrammar ::[String] -> IO ()
printGrammar file = do
    --grammar <- parseGrammarFile $ head file
    putStrLn "printGrammar"
    return ()
printConvertedGrammar ::[String] -> IO ()
printConvertedGrammar file = do
    --grammar = parseGrammarFile $ head file
    putStrLn "convert"
    return ()
printNFA ::[String] -> IO ()
printNFA file = do
    --grammar = parseGrammarFile $ head file
    putStrLn "make NFA"
    return ()

printError :: [String] -> IO ()
printError _ = do  
    putStrLn "Error"
    return ()

{-
parseGrammarFile :: String -> IO Grammar
parseGrammarFile fileName = if fileName == "#STDOUT" 
    then do
        putStrLn "stdout read" 
        contents <- getContents
        return parseGrammar contents
    else do
        withFile fileName ReadMode (\handle -> do
            putStrLn "parsing file"
            contents <- hGetContents handle
            return parseGrammar contents)
-}

parseGrammar :: String -> Grammar
parseGrammar contents = Grammar {nonTerminals = getNonTerminals $ rows !! 0
                                , terminals = getTerminals $ rows !! 1
                                , startNonTerminal = getStartNonTerminal $ rows !! 2
                                , rules = getRules $ drop 3 rows}
                            where rows = lines contents
        

getNonTerminals :: [Char] -> [Char]
getNonTerminals nonTerms = if all (\nonTerm -> length nonTerm == 1 && isUpperList nonTerm) $ splitOn "," nonTerms
                                then map head $ splitOn "," nonTerms
                                else error "wrong format of input - nonterminals"

getTerminals :: [Char] -> [Char]
getTerminals terms = if all (\term -> length term == 1 && isUpperList term) $ splitOn "," terms
                        then map head $ splitOn "," terms
                        else error "wrong format of input - terminals"

getStartNonTerminal :: [Char] -> Char
getStartNonTerminal nonTerms = if length nonTerms == 1 && isUpperList nonTerms
                                    then head nonTerms
                                    else error "wrong format of input - start nonterminal"

getRules :: [String] -> [(Char, [Char])]
getRules [] = []
getRules (rule:rules) = if length (ruleParts!!0) == 1 && isUpperList (ruleParts!!0) && all (\symbol -> isLower symbol || isUpper symbol) (ruleParts!!1)
                            then (head ruleParts!!0, ruleParts!!1):getRules rules 
                            else error "wrong format of input - rules"
                        where ruleParts = splitOn "->" rule
    
isUpperList :: String -> Bool
isUpperList list = all (isUpper) list 

isLowerList :: String -> Bool
isLowerList list = all (isLower) list
