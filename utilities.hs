-- Helper functions that don't really belong anywhere else
module Utilities where

import Data.Char
import Data.List

import DataStructure


-- Initial substring search - returns all matching entries
findPerson :: PhoneBook -> Name -> [Person]
findPerson pb nm = (filter (\x -> (map toLower $ take (length nm) (name x)) == map toLower nm) pb)


-- Returns whether any matches exist for the given initial substring
inPhoneBook :: PhoneBook -> Name -> Bool
inPhoneBook pb nm = length (findPerson pb nm) > 0


-- Print out multiple lines to the console
putLines :: [String] -> IO()
putLines [] = return ()
putLines (line:lines) = do
    putStrLn line
    putLines lines
    

-- Capitalize a string
capitalize :: String -> String
capitalize str = (toUpper (str!!0)):(tail str) 


-- Build a numbered list of people
listNames :: PhoneBook -> [String]
listNames = listNamesRec 0 -- Call through to the recursive function with index 0

listNamesRec :: Integer -> PhoneBook -> [String]
listNamesRec _ [] = []
listNamesRec ix (person:people) = ((show (ix + 1)) ++ ": " ++ (name person)):listNamesRec (ix + 1) people


-- Build a numbered list of a person's phone numbers
listPhones :: Person -> [String]
listPhones person = listPhonesRec 0 (phones person) -- Call through to the recursive function with index 0

listPhonesRec :: Integer -> Phones -> [String]
listPhonesRec _ [] = []
listPhonesRec ix (phone:phones) = ((show (ix + 1)) ++ ": " ++ (capitalize $ fst phone) ++ " - " ++ (snd phone)):listPhonesRec (ix + 1) phones


sortPhones :: Phones -> Phones
sortPhones ps = sortBy (\x y -> compare (fst x) (fst y)) ps