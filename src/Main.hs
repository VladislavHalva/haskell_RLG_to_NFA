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
import Data.List (intercalate, nub)
import Data.Char (isUpper, isLower, isAlpha)
import Data.Unique

data Grammar = Grammar  { nonTerminals :: String
                        , terminals :: String
                        , startNonTerminal :: Char
                        , rules :: [(Char, String)]
                        }

instance Show Grammar where
    show (Grammar nt t snt r) = " G = ({" ++ nt ++ "}, {" ++ t ++ "}, " ++ [snt] ++ ", {" ++ printRules r ++ "})"  
        where
            printRules x = init $ init $ parseRules x -- this line removes the comma and space at the end of rules
            parseRules [] = [] 
            parseRules ((ruleLeft, ruleRight):restRules) = [ruleLeft] ++ "->" ++ ruleRight ++ ", " ++ parseRules restRules


data FiniteAutomaton = FA   { states :: [Integer]
                            , startState :: Integer
                            , finishStates :: [Integer]
                            , transitions :: [(Integer, Char, Integer)]
                            }
    deriving Show


convertToRegularGrammar :: Grammar -> Grammar
convertToRegularGrammar (Grammar nt t snt r) =  Grammar (nt ++ newNt) t snt newRules
    where (newNt, newRules) = convertToRegularRules r

convertToRegularRules :: [(Char, String)] -> (String, [(Char, String)])
convertToRegularRules r = (getNonTerminalsFromRules newRules, newRules)
    where newRules = getNewRules r

getNewRules :: [(Char, String)] -> [(Char, String)]
getNewRules [] = []
getNewRules ((left, right):rs)   | isLowerList right = breakRuleWithTermsOnly left right ++ getNewRules rs --just terms
                                    | length right == 1 && head right == '#' = [] ++ getNewRules rs --epsilon
                                    | otherwise = [] ++ getNewRules rs -- terms and nonterm

breakRuleWithTermsOnly :: Char -> String -> [(Char, String)]
breakRuleWithTermsOnly left right = (left, [head right]) : [('a', "")]


getNonTerminalsFromRules :: [(Char, String)] -> String
getNonTerminalsFromRules [] = []
getNonTerminalsFromRules ((left, right):rs) = nub $ left : filter isUpper right ++ getNonTerminalsFromRules rs

{-convertToRegularRules ((left, right):rules) = 
    if length $ filter isLower right > 1
        then if length $ filter isUpper right > 0
            then convertRuleWithNonTerm (left, right) : convertToRegularRules rules
            else convertRuleWithoutNonTerm (left, right) : convertToRegularRules rules
        else ([],[], (left,right)) : convertToRegularRules rules

    
convertRuleWithNonTerm :: (Char, [Char]) ->      
-}

main :: IO ()
main = do
    (option:fileName) <- getArgs 
    operation <- procArgs option fileName
    operation $ setStdOutIfNotDefined fileName
    return ()


-- processes arguments from commandline, return operation to be executed
procArgs :: (Monad m) => String -> [String] -> m ([String] -> IO ())
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
setStdOutIfNotDefined list = if null list 
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

test :: String
test = "A,B\na,b,c\nA\nA->aaB\nA->ccB\nB->bB\nB->#"

parseGrammar :: String -> Grammar
parseGrammar contents = Grammar {nonTerminals = getNonTerminalsParser $ head rows
                                , terminals = getTerminalsParser $ rows !! 1
                                , startNonTerminal = getStartNonTerminal $ rows !! 2
                                , rules = getRulesParser $ drop 3 rows}
                            where rows = lines contents
        

getNonTerminalsParser :: String -> String
getNonTerminalsParser nonTerms = if all (\nonTerm -> length nonTerm == 1 && isUpperList nonTerm) $ splitOn "," nonTerms
                                then map head $ splitOn "," nonTerms
                                else error "wrong format of input - nonterminals"

getTerminalsParser :: String -> String
getTerminalsParser terms = if all (\term -> length term == 1 && isLowerList term) $ splitOn "," terms
                        then map head $ splitOn "," terms
                        else error "wrong format of input - terminals"

getStartNonTerminal :: String -> Char
getStartNonTerminal nonTerms = if length nonTerms == 1 && isUpperList nonTerms
                                    then head nonTerms
                                    else error "wrong format of input - start nonterminal"

getRulesParser :: [String] -> [(Char, String)]
getRulesParser [] = []
getRulesParser (rule:rs) = if  length (head ruleParts) == 1 && isUpperList (head ruleParts) 
                            && isLetterOrHashList (ruleParts!!1)
                                then (head $ head ruleParts, ruleParts!!1):getRulesParser rs 
                                else error "wrong format of input - rules"
                        where ruleParts = splitOn "->" rule
    
isUpperList :: String -> Bool
isUpperList = all isUpper 

isLowerList :: String -> Bool
isLowerList = all isLower

isLetterOrHashList :: String -> Bool
isLetterOrHashList list = all isAlpha list
                        || (length list == 1 && head list == '#')
