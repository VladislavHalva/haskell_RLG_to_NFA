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

{-Structures ####################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

data Grammar = Grammar  { nonTerminals :: String
                        , terminals :: String
                        , startNonTerminal :: String
                        , rules :: [(String, String)]
                        }

instance Show Grammar where
    show (Grammar nt t snt r) = " G = ({" ++ nt ++ "}, {" ++ t ++ "}, " ++ snt ++ ", {" ++ printRules r ++ "})"  
        where
            printRules x = init $ init $ parseRules x -- this line removes the comma and space at the end of rules
            parseRules [] = [] 
            parseRules ((ruleLeft, ruleRight):restRules) = ruleLeft ++ "->" ++ ruleRight ++ ", " ++ parseRules restRules


data FiniteAutomaton = FA   { states :: [Integer]
                            , startState :: Integer
                            , finishStates :: [Integer]
                            , transitions :: [(Integer, Char, Integer)]
                            }
    deriving Show


{-Converting part################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

convertToRegularGrammar :: Grammar -> Grammar
convertToRegularGrammar (Grammar nt t snt r) =  Grammar (nt ++ newNt) t snt newRules
    where (newNt, newRules) = convertToRegularRules r

convertToRegularRules :: [(String, String)] -> (String, [(String, String)])
convertToRegularRules r = (getNonTerminalsFromRules newRules, newRules)
    where newRules = getNewRules r 0

getNewRules :: [(String, String)] -> Integer -> [(String, String)]
getNewRules [] _ = []
getNewRules ((left, right):rs) idx = brokenRules ++ getNewRules rs newIdx
    where (brokenRules, newIdx) = breakRule left right idx

breakRule :: String -> String -> Integer -> ([(String, String)], Integer)
breakRule left right idx    | isLowerList right = breakRuleWithTermsOnly left right idx
                            | length right == 1 && head right == '#' = ([(left, right)], idx)
                            | otherwise = breakRuleWithNonTerm left right idx

breakRuleWithTermsOnly :: String -> String -> Integer -> ([(String, String)], Integer)
breakRuleWithTermsOnly left right idx = ((left, [head right, 'A'] ++ show idx) : brokenRules ++ [("A" ++ show (maxIdx+1), "#")], maxIdx+1)
        where (brokenRules, maxIdx)  = getRulesAndMaxIndex $ breakChainOfTerms (tail right, idx+1)

breakRuleWithNonTerm :: String -> String -> Integer -> ([(String, String)], Integer)
breakRuleWithNonTerm left right idx = if isLower $ head $ tail right
        then ((left, [head right, 'A'] ++ show idx) : brokenRules, maxIdx+1)   
        else ([addLastRuleOfNonTermEndedChain right], maxIdx)
    where (brokenRules, maxIdx) = getRulesAndMaxIndex $ breakChainOfTermsWithNonTerm (tail right, idx+1) 

addLastRuleOfNonTermEndedChain :: String -> (String, String)
addLastRuleOfNonTermEndedChain x = ([head x], [last x])

getRulesAndMaxIndex :: [(String, String)] -> ([(String, String)], Integer)
getRulesAndMaxIndex rs = (rs, maxIndex $ last rs)

maxIndex :: (String, String) -> Integer
maxIndex (_, right) = read $ dropWhile isAlpha right :: Integer

breakChainOfTerms :: (String, Integer) -> [(String, String)]
breakChainOfTerms ([], idx) = []
breakChainOfTerms ((t:ts), idx) = ("A" ++ (show $ idx-1), [t] ++ show idx) : breakChainOfTerms (ts, idx+1)

breakChainOfTermsWithNonTerm :: (String, Integer) -> [(String, String)]
breakChainOfTermsWithNonTerm ((t:ts), idx) = if length ts > 1
    then ([t], "A" ++ show idx) : breakChainOfTermsWithNonTerm (ts, idx+1) 
    else [addLastRuleOfNonTermEndedChain (t:ts)]

getNonTerminalsFromRules :: [(String, String)] -> String
getNonTerminalsFromRules [] = []
getNonTerminalsFromRules ((left, right):rs) = nub $ left ++ filter isUpper right ++ getNonTerminalsFromRules rs


{-Main ##########################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

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



{-Parser ########################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}



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

getStartNonTerminal :: String -> String
getStartNonTerminal nonTerms = if length nonTerms == 1 && isUpperList nonTerms
                                    then [head nonTerms]
                                    else error "wrong format of input - start nonterminal"

getRulesParser :: [String] -> [(String, String)]
getRulesParser [] = []
getRulesParser (rule:rs) = if  length (head ruleParts) == 1 && isUpperList (head ruleParts) 
                            && isLetterOrHashList (ruleParts!!1)
                                then ([head $ head ruleParts], ruleParts!!1):getRulesParser rs 
                                else error "wrong format of input - rules"
                        where ruleParts = splitOn "->" rule
    
{-Additional ####################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

isUpperList :: String -> Bool
isUpperList = all isUpper 

isLowerList :: String -> Bool
isLowerList = all isLower

isLetterOrHashList :: String -> Bool
isLetterOrHashList list = all isAlpha list
                        || (length list == 1 && head list == '#')
