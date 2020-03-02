module StructuresG2FA where

import Data.List (intersperse)


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