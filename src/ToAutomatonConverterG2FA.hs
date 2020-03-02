{- FIT VUT Brno, FLP 2019/2020 
 - Assignment: PLG-2-NKA
 - Author: Vladislav Halva
 - Login: xhalva04
 -}

module ToAutomatonConverterG2FA (convertToFiniteAutomaton) where 

import StructuresG2FA
import ParserG2FA (parseGrammar)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

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
nonTerminalsToStatesMap ns = Map.fromList $ nonTermsStatesPairs ns 0

-- returns nonterminal - unique interger pairs
-- assigning starts from the given number
nonTermsStatesPairs :: [String] -> Integer -> [(String, Integer)]
nonTermsStatesPairs [] _ = []
nonTermsStatesPairs (n:ns) st = (n, st) : nonTermsStatesPairs ns (st+1)