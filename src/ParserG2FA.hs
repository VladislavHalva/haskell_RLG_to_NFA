{- FIT VUT Brno, FLP 2019/2020 
 - Assignment: PLG-2-NKA
 - Author: Vladislav Halva
 - Login: xhalva04
 -}
module ParserG2FA (parseGrammar) where

import StructuresG2FA
import AdditionalG2FA (isUpperList, isLowerList)

import Data.List.Split (splitOn)
import Data.Maybe (isNothing, fromJust)
import Data.Char (isUpper, isLower)

-- takes a string representing a grammar and converts it to an internal representation
-- checks if it is syntactically corrent, not semantically !!
parseGrammar :: String -> Grammar
parseGrammar contents = 
    if isNothing g 
        then error "incorrect grammar as input, used terminals and nonterminals not defined"    
        else fromJust g              
    where 
        g = checkGrammarSymbols Grammar {nonTerminals = getNonTerminalsParser $ head rows
        , terminals = getTerminalsParser $ rows !! 1
        , startNonTerminal = getStartNonTerminalParser $ rows !! 2
        , rules = getRulesParser $ drop 3 rows}
        rows = lines contents
        
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
getStartNonTerminalParser :: String -> String
getStartNonTerminalParser nonTerm = 
    if length nonTerm == 1 && isUpperList nonTerm
        then nonTerm
        else error "wrong format of input - start nonterminal"

-- returns list of rules parsed from string representation
getRulesParser :: [String] -> [(String, [String])]
getRulesParser [] = []
getRulesParser (rule:rs) = 
    if  length(head ruleParts) == 1 && isUpperList (head ruleParts) && isValidRule(ruleParts!!1)
        then ([head $ head ruleParts], splitRightParser $ ruleParts!!1 ) : getRulesParser rs 
        else error "wrong format of input - rules"
    where ruleParts = splitOn "->" rule
    
-- splits right side of a rule to a list of terminals and nonterminals 
-- each as a string consisting of one character
splitRightParser :: String -> [String]
splitRightParser [] = []
splitRightParser (x:xs) = [x] : splitRightParser xs

-- true if chain of lower cases, chain of lower cases followed by upper case, or hash
-- false otherwise
isValidRule :: String -> Bool
isValidRule list = 
    all isLower list
    || (all isLower (init list) && (isUpper $ last list))  
    || (length list == 1 && head list == '#')

checkGrammarSymbols :: Grammar -> Maybe Grammar
checkGrammarSymbols gr@(Grammar nt t snt r) = 
    if checkRulesSymbols nt t r && checkStartNontermSymbol nt snt
        then Just gr
        else Nothing

checkStartNontermSymbol :: [String] -> String -> Bool
checkStartNontermSymbol nt snt = elem snt nt

checkRulesSymbols :: [String] -> [String] -> [(String, [String])] -> Bool
checkRulesSymbols _ _ [] = True
checkRulesSymbols nt t (r:rs) = (checkRuleSymbols nt t r) && (checkRulesSymbols nt t rs) 

checkRuleSymbols :: [String] -> [String] -> (String, [String]) -> Bool
checkRuleSymbols nt t (left, right) = 
    (elem left nt) && 
    all (\c -> (elem c nt) || (elem c t) || c == "#") right   
