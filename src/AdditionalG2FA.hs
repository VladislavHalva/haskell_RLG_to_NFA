module AdditionalG2FA (isUpperList, isLowerList, isUppersAndDigitsString, containsNumString) where

import Data.Char (isUpper, isLower, isDigit)

isUpperList :: String -> Bool
isUpperList = all isUpper 

isLowerList :: String -> Bool
isLowerList = all isLower

isUppersAndDigitsString :: String -> Bool
isUppersAndDigitsString str = all (\x -> isDigit x || isUpper x) str

containsNumString :: String -> Bool
containsNumString str = any isDigit str