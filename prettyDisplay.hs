-- Handles readable output of phone book entries
module PrettyDisplay where

import Data.Char
import DataStructure


capitalize :: String -> String
capitalize str = toUpper (str!!0):(tail str) 


-- IO: Recursive handler for displaying the entire phone book

showPhoneBook :: PhoneBook -> IO()
showPhoneBook [] = do
    putStrLn "-------------------------" -- End of phone book delimiter
    putStrLn ""

showPhoneBook (person:people) = do
    showPerson person
    showPhoneBook people
    

-- IO: Prettified display for an individual person

showPerson :: Person -> IO()
showPerson person = do
    putStrLn "-------------------------" -- delimiter
    putStrLn ""

    putStrLn $ name person -- Assume that there is always a name
    
    showPhones $ phones person
    
    -- Put a line break between the phone numbers and the address if both are present
    if ((length $ phones person) > 0 && (length $ address person) > 0) then
        putStrLn ""
    else putStr ""
    
    showAddress $ address person
    
    if (dob person /= "") then do
        putStrLn ""
        putStrLn $ "DoB: " ++ dob person
        putStrLn ""
    else putStrLn ""

        
-- IO: Recursively show all phones as 'description: number'

showPhones :: Phones -> IO()
showPhones [] = do { putStr "" }
showPhones (phone:phones) = do
    putStrLn $ "  " ++ (capitalize $ fst phone) ++ ": " ++ (snd phone)
    showPhones phones


-- IO: Recursively show all lines in the address

showAddress :: Address -> IO()
showAddress [] = do { putStr "" }
showAddress (line:address) = do
    putStrLn $ "  " ++ snd line
    showAddress address
    

-- IO: Display a numbered list of people

listNames :: PhoneBook -> IO()
listNames = listNamesRec 0 -- Call through to the recursive function with index 0

listNamesRec :: Integer -> PhoneBook -> IO ()
listNamesRec _ [] = putStr ""
listNamesRec ix (person:people) = do
    putStrLn $ (show (ix + 1)) ++ ": " ++ (name person)
    listNamesRec (ix + 1) people
