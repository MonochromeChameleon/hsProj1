-- Handles readable output of phone book entries
module PrettyDisplay where

import DataStructure


-- Recursive handler for displaying the entire phone book
showPhoneBook :: PhoneBook -> IO()
showPhoneBook [] = do
    putStrLn "-------------------------" -- End of phone book delimiter
    putStrLn ""

showPhoneBook (person:people) = do
    showPerson person
    showPhoneBook people
    

-- Prettified display for an individual person
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
        
-- Recursively show all phones as 'description: number'
showPhones :: Phones -> IO()
showPhones [] = do { putStr "" }
showPhones (phone:phones) = do
    putStrLn $ "  " ++ (fst phone) ++ ": " ++ (snd phone)
    showPhones phones

-- Recursively show all lines in the address
showAddress :: Address -> IO()
showAddress [] = do { putStr "" }
showAddress (line:address) = do
    putStrLn $ "  " ++ snd line
    showAddress address