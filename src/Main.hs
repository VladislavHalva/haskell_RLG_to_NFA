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
import Data.List (intercalate, nub, intersperse)
import Data.Char (isUpper, isLower, isAlpha, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map

{-Structures ####################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

data Grammar = Grammar  { nonTerminals :: [String]
                        , terminals :: [String]
                        , startNonTerminal :: String
                        , rules :: [(String, [String])]
                        }

instance Show Grammar where
    show (Grammar nt t snt r) = " G = ({" ++ intersperseListOfLists nt 
        ++ "}, {" ++ intersperseListOfLists t ++ "}, " 
        ++ snt ++ ", {" ++ printRules r ++ "})"  
        where
            printRules x = init $ init $ parseRules x -- this line removes the comma and space at the end of rules
            parseRules [] = [] 
            parseRules ((ruleLeft, ruleRight):restRules) = ruleLeft ++ "->" ++ concat ruleRight ++ ", " ++ parseRules restRules
            intersperseListOfLists [] = []
            intersperseListOfLists (x:xs) = if length xs /= 0
                then x ++ "," ++ intersperseListOfLists xs  
                else x

data FiniteAutomaton = FA   { states :: [Integer]
                            , alphabet :: String
                            , transitions :: [(Integer, Char, Integer)]
                            , startState :: Integer
                            , finishStates :: [Integer]
                            }

instance Show FiniteAutomaton where
    show (FA st alph t sSt fSt) = "FA = ({" ++ init ( tail (show st)) ++ "}, {" 
        ++ intersperse ',' alph ++ "}, {" ++ printTransitions t ++ "}, " 
        ++ show sSt ++ ", {" ++ init ( tail (show fSt)) ++ "})"
        where
            printTransitions ts = init $ init $ parseTransitions ts -- this line removes comma and space at the end of transitions
            parseTransitions [] = []
            parseTransitions ((from, with, to):ts) = show from ++ [with] ++ "->" ++ show to ++ ", " ++ parseTransitions ts 

{-Converting to Finite automaton part############################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}
{-
--expects regular grammar as an input
convertToFiniteAutomaton :: Grammar -> FiniteAutomaton
convertToFiniteAutomaton (Grammar nt t snt r) = FA  { states = getStates mapping $ ntsToListOfNts nt
                                                    , alphabet = t
                                                    , transitions = getTransitions mapping r
                                                    , startState = getState mapping snt
                                                    , finishStates = getNtsInEpsilonRules mapping r} 
                                                where mapping = nonTerminalsToStatesMap $ ntsToListOfNts nt


getTransitions :: Map String Integer -> [(String, String)] -> [(Integer, Char, Integer)]
getTransitions _ [] = []
getTransitions m ((left, right):rs) = if right /= "#"
    then (getState m left, head right, getState m $ tail right) : getTransitions m rs
    else getTransitions m rs

ntsToListOfNts :: String -> [String]
ntsToListOfNts [] = []
ntsToListOfNts (x:y:xs) = if isDigit y
        then ([x,y] ++ this) : ntsToListOfNts others  
        else [x] : ntsToListOfNts (y:xs)
    where (this, others) = break isAlpha xs 
ntsToListOfNts (x:_) = [[x]]

getNtsInEpsilonRules :: Map String Integer -> [(String, String)] -> [Integer]
getNtsInEpsilonRules _ [] = []
getNtsInEpsilonRules mapping ((left, right):rs) = if right == "#"
    then (getState mapping left) : getNtsInEpsilonRules mapping rs
    else getNtsInEpsilonRules mapping rs

getStates :: Map String Integer -> [String] -> [Integer]
getStates _ [] = []
getStates mapping (n:ns) = getState mapping n : getStates mapping ns

getState :: Map String Integer -> String -> Integer
getState mapping nt = fromJust $ Map.lookup nt mapping   

nonTerminalsToStatesMap :: [String] -> Map String Integer
nonTerminalsToStatesMap ns = Map.fromList $ nonTermsStatesPairs ns 1

nonTermsStatesPairs :: [String] -> Integer -> [(String, Integer)]
nonTermsStatesPairs [] _ = []
nonTermsStatesPairs (n:ns) st = (n, st) : nonTermsStatesPairs ns (st+1)

-}

{-Converting to Regular part#####################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

convertToRegularGrammar :: Grammar -> Grammar
convertToRegularGrammar (Grammar nt t snt r) =  Grammar newNt t snt newRules
    where (newNt, newRules) = convertToRegularRules r

convertToRegularRules :: [(String, [String])] -> ([String], [(String, [String])])
convertToRegularRules r = (getNonTerminalsFromRules newRules, newRules)
    where newRules = getNewRules r 0

getNewRules :: [(String, [String])] -> Integer -> [(String, [String])]
getNewRules [] _ = []
getNewRules ((left, right):rs) idx = brokenRules ++ getNewRules rs (newIdx + 1)
    where (brokenRules, newIdx) = breakRule left right idx

breakRule :: String -> [String] -> Integer -> ([(String, [String])], Integer)
breakRule left right idx    | isLowerList $ concat right = breakRuleWithTermsOnly left right idx
                            | length right == 1 && head right == "#" = ([(left, right)], idx)
                            | otherwise = breakRuleWithNonTerm left right idx

breakRuleWithTermsOnly :: String -> [String] -> Integer -> ([(String, [String])], Integer)
breakRuleWithTermsOnly left right idx = if length right > 1
    then ((left, [head right] ++ ["A" ++ show idx]) : brokenRules ++ [("A" ++ show maxIdx, ["#"])], maxIdx+1)
    else ([(left, right)], idx)
        where (brokenRules, maxIdx)  = getRulesAndMaxIndex $ breakChainOfTerms (tail right, idx+1)

breakRuleWithNonTerm :: String -> [String] -> Integer -> ([(String, [String])], Integer)
breakRuleWithNonTerm left right idx = if length right > 2
    then ((left, [head right] ++ ["A" ++ show idx]) : brokenRules, maxIdx+1)  
    else ([(left, right)], idx)
    where (brokenRules, maxIdx) = getRulesAndMaxIndex $ breakChainOfTermsWithNonTerm (tail right, idx) 

getRulesAndMaxIndex :: [(String, [String])] -> ([(String, [String])], Integer)
getRulesAndMaxIndex rs = (rs, maxIndex $ rs)

maxIndex :: [(String, [String])] -> Integer
maxIndex rs = maximum $ getIndexesOfNonTerms $ getRightPartsOfRules rs

getRightPartsOfRules ::  [(String, [String])] -> [String]
getRightPartsOfRules [] = []
getRightPartsOfRules ((_, right):rs) = right ++ getRightPartsOfRules rs

getIndexesOfNonTerms :: [String] -> [Integer]
getIndexesOfNonTerms [] = []
getIndexesOfNonTerms (r:rs) = if containsNumString r
    then (read $ dropWhile isAlpha r :: Integer) : getIndexesOfNonTerms rs
    else -1 : getIndexesOfNonTerms rs

breakChainOfTerms :: ([String], Integer) -> [(String, [String])]
breakChainOfTerms ([], idx) = []
breakChainOfTerms ((t:ts), idx) = ("A" ++ (show $ idx-1), [t] ++ ["A" ++ show idx]) : breakChainOfTerms (ts, idx+1)

breakChainOfTermsWithNonTerm :: ([String], Integer) -> [(String, [String])]
breakChainOfTermsWithNonTerm ([], _) = []
breakChainOfTermsWithNonTerm ((t:ts), idx) = if length ts > 1
    then ("A" ++ show idx, [t] ++ ["A" ++ show (idx+1)]) : breakChainOfTermsWithNonTerm (ts, idx+1) 
    else [addLastRuleOfNonTermEndedChain (t:ts) idx]

addLastRuleOfNonTermEndedChain :: [String] -> Integer -> (String, [String])
addLastRuleOfNonTermEndedChain x idx = ("A" ++ show idx, x)

getNonTerminalsFromRules :: [(String, [String])] -> [String]
getNonTerminalsFromRules [] = []
getNonTerminalsFromRules ((left, right):rs) = nub $ [left] ++ filter isUppersAndDigitsString right ++ getNonTerminalsFromRules rs


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
test = "A,B\na,b,c,d,e\nA\nA->aeabB\nA->cdB\nB->eB\nB->#"

parseGrammar :: String -> Grammar
parseGrammar contents = Grammar {nonTerminals = getNonTerminalsParser $ head rows
                                , terminals = getTerminalsParser $ rows !! 1
                                , startNonTerminal = getStartNonTerminal $ rows !! 2
                                , rules = getRulesParser $ drop 3 rows}
                            where rows = lines contents
        

getNonTerminalsParser :: String -> [String]
getNonTerminalsParser nonTerms = if all (\nonTerm -> length nonTerm == 1 && isUpperList nonTerm) $ splitOn "," nonTerms
                                then splitOn "," nonTerms
                                else error "wrong format of input - nonterminals"

getTerminalsParser :: String -> [String]
getTerminalsParser terms = if all (\term -> length term == 1 && isLowerList term) $ splitOn "," terms
                        then splitOn "," terms
                        else error "wrong format of input - terminals"

getStartNonTerminal :: String -> String
getStartNonTerminal nonTerms = if length nonTerms == 1 && isUpperList nonTerms
                                    then [head nonTerms]
                                    else error "wrong format of input - start nonterminal"

getRulesParser :: [String] -> [(String, [String])]
getRulesParser [] = []
getRulesParser (rule:rs) = if  length (head ruleParts) == 1 && isUpperList (head ruleParts) 
                            && isLetterOrHashList (ruleParts!!1)
                                then ([head $ head ruleParts], splitRightParser $ ruleParts!!1 ) : getRulesParser rs 
                                else error "wrong format of input - rules"
                        where ruleParts = splitOn "->" rule
    
splitRightParser :: String -> [String]
splitRightParser [] = []
splitRightParser (x:xs) = [x] : splitRightParser xs

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

isUppersAndDigitsString :: String -> Bool
isUppersAndDigitsString str = all (\x -> isDigit x || isUpper x) str

containsNumString :: String -> Bool
containsNumString str = any isDigit str


