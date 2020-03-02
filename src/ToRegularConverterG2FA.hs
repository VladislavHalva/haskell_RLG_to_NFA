{- FIT VUT Brno, FLP 2019/2020 
 - Assignment: PLG-2-NKA
 - Author: Vladislav Halva
 - Login: xhalva04
 -}

module ToRegularConverterG2FA (convertToRegularGrammar) where

import StructuresG2FA
import AdditionalG2FA

import Data.List (nub)
import Data.Char (isUpper, isLower, isAlpha)

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
maxIndex rs = maximum $ getIndexesOfNonTerms (getRightPartsOfRules rs ++ getLeftPartsOfRules rs)

-- returns list of right sides of given list of rules
getRightPartsOfRules ::  [(String, [String])] -> [String]
getRightPartsOfRules [] = []
getRightPartsOfRules ((_, right):rs) = right ++ getRightPartsOfRules rs

-- returns list of left sides of given list of rules
getLeftPartsOfRules :: [(String, [String])] -> [String]
getLeftPartsOfRules [] = []
getLeftPartsOfRules ((left,_):rs) = [left] ++ getRightPartsOfRules rs

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
