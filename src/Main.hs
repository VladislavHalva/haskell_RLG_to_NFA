{- FIT VUT v Brně, FLP 2019/2020 
 -Zadani projektu: PLG-2-NKA
 -Autor: Vladislav Halva
 -Login: xhalva04
-}

module Main (main) where

import System.Environment (getArgs)
import System.IO (getContents)
import Data.Maybe (isNothing, fromJust)
import Data.List.Split (splitOn)
import Data.List (nub, intersperse)
import Data.Char (isUpper, isLower, isAlpha, isDigit)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit (die)

{-Structures ####################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

-- datastructure representing a grammar
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
            printRules [] = [] 
            printRules ((ruleLeft, ruleRight):restRules) = if restRules == []
                then ruleLeft ++ "->" ++ concat ruleRight
                else ruleLeft ++ "->" ++ concat ruleRight ++ ", " ++ printRules restRules
            intersperseListOfLists [] = []
            intersperseListOfLists (x:xs) = if length xs /= 0
                then x ++ "," ++ intersperseListOfLists xs  
                else x

-- datastructure representing a finite automaton
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
            printTransitions [] = []
            printTransitions ((from, with, to):ts) = if ts == []
                then show from ++ [with] ++ "->" ++ show to
                else show from ++ [with] ++ "->" ++ show to ++ ", " ++ printTransitions ts 


{-Main ##########################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}


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


{-Converting to Finite automaton part############################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}


-- constructs finite automaton that excepts a langage
-- specified by the given grammar
-- !! expects right regular grammar as an input
convertToFiniteAutomaton :: Grammar -> FiniteAutomaton
convertToFiniteAutomaton (Grammar nt t snt r) = 
    FA  { states = getStates mapping nt
        , alphabet = concat t
        , transitions = getTransitions mapping r
        , startState = getState mapping snt
        , finishStates = getNtsInEpsilonRules mapping r} 
    where mapping = nonTerminalsToStatesMap nt

getTransitions :: Map String Integer -> [(String, [String])] -> [(Integer, Char, Integer)]
getTransitions _ [] = []
getTransitions m ((left, right):rs) = if head right /= "#"
    then (getState m left, head $ head right, getState m $ head $ tail right) : getTransitions m rs
    else getTransitions m rs

-- return list of nonterminals that occur on the left side of epsilon rules
getNtsInEpsilonRules :: Map String Integer -> [(String, [String])] -> [Integer]
getNtsInEpsilonRules _ [] = []
getNtsInEpsilonRules mapping ((left, right):rs) = if head right == "#"
    then (getState mapping left) : getNtsInEpsilonRules mapping rs
    else getNtsInEpsilonRules mapping rs

-- returns list of states that correspond to given list of nonterminals
-- according to  given mapping
getStates :: Map String Integer -> [String] -> [Integer]
getStates _ [] = []
getStates mapping (n:ns) = getState mapping n : getStates mapping ns

-- returns a state that corresponds to given nonterminal
-- according to given mapping
getState :: Map String Integer -> String -> Integer
getState mapping nt = fromJust $ Map.lookup nt mapping   

-- creates mapping of nonterminals to states
-- ie. assigns an integer to each nonterminal
nonTerminalsToStatesMap :: [String] -> Map String Integer
nonTerminalsToStatesMap ns = Map.fromList $ nonTermsStatesPairs ns 1

-- returns nonterminal - unique interger pairs
-- assigning starts from the given number
nonTermsStatesPairs :: [String] -> Integer -> [(String, Integer)]
nonTermsStatesPairs [] _ = []
nonTermsStatesPairs (n:ns) st = (n, st) : nonTermsStatesPairs ns (st+1)


{-Converting to Regular part#####################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

-- converts the given grammar to a regular form
convertToRegularGrammar :: Grammar -> Grammar
convertToRegularGrammar (Grammar _ t snt r) =  Grammar newNt t snt newRules
    where (newNt, newRules) = convertToRegularRules r

-- returns new right regular rules and the new set of used nonterminals as a list
convertToRegularRules :: [(String, [String])] -> ([String], [(String, [String])])
convertToRegularRules r = (getNonTerminalsFromRules newRules, newRules)
    where newRules = getNewRules r 0

-- converts rules of a given grammar to regular ones
-- newly created nonterminals are indexed from the given number
getNewRules :: [(String, [String])] -> Integer -> [(String, [String])]
getNewRules [] _ = []
getNewRules ((left, right):rs) idx = brokenRules ++ getNewRules rs (newIdx + 1)
    where (brokenRules, newIdx) = breakRule left right idx

-- if a rule is regular - it is just returned
-- if a rule needs to be broken to a set of rules (chain) it is, and the new rules are returned
breakRule :: String -> [String] -> Integer -> ([(String, [String])], Integer)
breakRule left right idx    | isLowerList $ concat right = breakRuleWithTermsOnly left right idx
                            | length right == 1 && head right == "#" = ([(left, right)], idx)
                            | otherwise = breakRuleWithNonTerm left right idx

-- breaks a rule consisting of more than one terminal on the right side to a chain of 
-- new rules
breakRuleWithTermsOnly :: String -> [String] -> Integer -> ([(String, [String])], Integer)
breakRuleWithTermsOnly left right idx = if length right > 1
    then ((left,[head right]++["A"++show idx]):brokenRules ++ [("A"++show maxIdx,["#"])], maxIdx+1)
    else ([(left, right)], idx)
        where (brokenRules, maxIdx)  = getRulesAndMaxIndex $ breakChainOfTerms (tail right, idx+1)

-- breaks a rule consisting of more than one terminal and a nonterminal at the end to 
-- a chain of new rules 
breakRuleWithNonTerm :: String -> [String] -> Integer -> ([(String, [String])], Integer)
breakRuleWithNonTerm left right idx = if length right > 2
    then ((left, [head right] ++ ["A" ++ show idx]) : brokenRules, maxIdx+1)  
    else ([(left, right)], idx)
    where (brokenRules, maxIdx) = getRulesAndMaxIndex $ breakChainOfTermsWithNonTerm(tail right,idx) 

-- returns maximum index value of a nonterminal in given list of rules and the rules unmodified
getRulesAndMaxIndex :: [(String, [String])] -> ([(String, [String])], Integer)
getRulesAndMaxIndex rs = (rs, maxIndex $ rs)

-- returns maximum index value of a nonterminal in given list of rules
maxIndex :: [(String, [String])] -> Integer
maxIndex rs = maximum $ getIndexesOfNonTerms $ getRightPartsOfRules rs

-- returns list of right sides of given list of rules
getRightPartsOfRules ::  [(String, [String])] -> [String]
getRightPartsOfRules [] = []
getRightPartsOfRules ((_, right):rs) = right ++ getRightPartsOfRules rs

-- returns list of indexes of all nonterms in given strings
getIndexesOfNonTerms :: [String] -> [Integer]
getIndexesOfNonTerms [] = []
getIndexesOfNonTerms (r:rs) = if containsNumString r
    then (read $ dropWhile isAlpha r :: Integer) : getIndexesOfNonTerms rs
    else -1 : getIndexesOfNonTerms rs

-- breaks a chain of terms on the right side of a rule a set of new rules containing
-- newly created nonterminals
breakChainOfTerms :: ([String], Integer) -> [(String, [String])]
breakChainOfTerms ([], _) = []
breakChainOfTerms ((t:ts),idx) = 
    ("A" ++ (show $ idx-1), [t] ++ ["A" ++ show idx]) : breakChainOfTerms (ts,idx+1)

-- breaks a chain of terms ended with a nonterminal on the right side of a rule a set of 
-- new rules containing newly created nonterminals
breakChainOfTermsWithNonTerm :: ([String], Integer) -> [(String, [String])]
breakChainOfTermsWithNonTerm ([], _) = []
breakChainOfTermsWithNonTerm ((t:ts), idx) = if length ts > 1
    then ("A" ++ show idx, [t] ++ ["A" ++ show (idx+1)]) : breakChainOfTermsWithNonTerm (ts, idx+1) 
    else [addLastRuleOfNonTermEndedChain (t:ts) idx]

-- creates the last rule of a set of rules that replace the rule with a chain of terminals
-- ended with a terminal (e.g. A3 -> bB from a rule A -> aabB)
addLastRuleOfNonTermEndedChain :: [String] -> Integer -> (String, [String])
addLastRuleOfNonTermEndedChain x idx = ("A" ++ show idx, x)

-- returns all nonterminals used in a given set of rules
getNonTerminalsFromRules :: [(String, [String])] -> [String]
getNonTerminalsFromRules [] = []
getNonTerminalsFromRules ((left, right):rs) = 
    nub $ [left] ++ filter isUppersAndDigitsString right ++ getNonTerminalsFromRules rs


{-Parser ########################################################################
#################################################################################
#################################################################################
#################################################################################
#################################################################################-}

-- takes a string representing a grammar and converts it to an internal representation
-- checks if it is syntactically corrent, not semantically !!
parseGrammar :: String -> Grammar
parseGrammar contents = Grammar {nonTerminals = getNonTerminalsParser $ head rows
                                , terminals = getTerminalsParser $ rows !! 1
                                , startNonTerminal = getStartNonTerminal $ rows !! 2
                                , rules = getRulesParser $ drop 3 rows}
                            where rows = lines contents
        
-- returns list of nonterminals parsed from string representation
getNonTerminalsParser :: String -> [String]
getNonTerminalsParser nonTerms = 
    if all (\nonTerm -> length nonTerm == 1 && isUpperList nonTerm) $ splitOn "," nonTerms
        then splitOn "," nonTerms
        else error "wrong format of input - nonterminals"

-- returns list of terminals parsed from string representation
getTerminalsParser :: String -> [String]
getTerminalsParser terms = 
    if all (\term -> length term == 1 && isLowerList term) $ splitOn "," terms
        then splitOn "," terms
        else error "wrong format of input - terminals"

-- returns nonterminal parsed from string representation
-- checks if there is only one specified 
getStartNonTerminal :: String -> String
getStartNonTerminal nonTerms = 
    if length nonTerms == 1 && isUpperList nonTerms
        then [head nonTerms]
        else error "wrong format of input - start nonterminal"

-- returns list of rules parsed from string representation
getRulesParser :: [String] -> [(String, [String])]
getRulesParser [] = []
getRulesParser (rule:rs) = 
    if  length(head ruleParts) == 1 && isUpperList (head ruleParts) && isLettersOrHash(ruleParts!!1)
        then ([head $ head ruleParts], splitRightParser $ ruleParts!!1 ) : getRulesParser rs 
        else error "wrong format of input - rules"
    where ruleParts = splitOn "->" rule
    
-- splits right side of a rule to a list of terminals and nonterminals 
-- each as a string consisting of one character
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

isLettersOrHash :: String -> Bool
isLettersOrHash list = all isAlpha list
                        || (length list == 1 && head list == '#')

isUppersAndDigitsString :: String -> Bool
isUppersAndDigitsString str = all (\x -> isDigit x || isUpper x) str

containsNumString :: String -> Bool
containsNumString str = any isDigit str

test :: String
test = "A,B\na,b,c,d,e\nA\nA->aeabB\nA->cdB\nB->eB\nB->#"
