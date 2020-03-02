{- FIT VUT Brno, FLP 2019/2020 
 - Assignment: PLG-2-NKA
 - Author: Vladislav Halva
 - Login: xhalva04
 -}

module StructuresG2FA where

import Data.List (intercalate)


-- datastructure representing a grammar
data Grammar = Grammar  { nonTerminals :: [String]
                        , terminals :: [String]
                        , startNonTerminal :: String
                        , rules :: [(String, [String])]
                        }

instance Show Grammar where
    show (Grammar nt t snt r) = 
        intersperseListOfLists nt ++ "\n" ++
        intersperseListOfLists t  ++ "\n" ++
        snt ++ "\n" ++
        printRules r
        where
            intersperseListOfLists [] = []
            intersperseListOfLists (x:xs) = if length xs /= 0
                then x ++ "," ++ intersperseListOfLists xs  
                else x
            printRules [] = [] 
            printRules ((ruleLeft, ruleRight):restRules) =
                ruleLeft ++ "->" ++ concat ruleRight ++ "\n" ++ printRules restRules

{- WRITE AS A SET OF ELEMENTS ACCORDING TO MATH DEFINITION - GRAMMAR

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
-}

-- datastructure representing a finite automaton
data FiniteAutomaton = FA   { states :: [Integer]
                            , alphabet :: String
                            , transitions :: [(Integer, Char, Integer)]
                            , startState :: Integer
                            , finishStates :: [Integer]
                            }

instance Show FiniteAutomaton where
    show (FA st _ t sSt fSt) = 
        intercalate "," (map show st) ++ "\n" ++
        show sSt ++ "\n" ++ 
        intercalate "," (map show fSt) ++ "\n" ++
        printTransitions t
            where
                printTransitions [] = []
                printTransitions ((from, with, to):ts) = 
                    show from ++ "," ++ [with] ++ "," ++ show to ++ "\n" ++ printTransitions ts


{- WRITE AS A SET OF ELEMENTS ACCORDING TO MATH DEFINITION - FINITE AUTOMATON

instance Show FiniteAutomaton where
    show (FA st alph t sSt fSt) = "FA = ({" ++ init ( tail (show st)) ++ "}, {" 
        ++ intersperse ',' alph ++ "}, {" ++ printTransitions t ++ "}, " 
        ++ show sSt ++ ", {" ++ init ( tail (show fSt)) ++ "})"
        where
            printTransitions [] = []
            printTransitions ((from, with, to):ts) = if ts == []
                then show from ++ [with] ++ "->" ++ show to
                else show from ++ [with] ++ "->" ++ show to ++ ", " ++ printTransitions ts 
-}