module PersonDeleter where

import DataStructure
import XmlWriter
import SearchHandler
import UserInteraction
import PrettyDisplay


-- IO: Start delete process - prompt for a name and handle results appropriately

doDelete :: PhoneBook -> IO()
doDelete pb = do
    putStrLn "Which user do you want to delete?"    
    searchTerm <- prompt "Delete"

    let matchingPeople = findPerson pb searchTerm
    if (length matchingPeople > 0) then
        verifyDelete pb matchingPeople
    else
        putStrLn "No matches found"

        
-- IO: Check whether more than one name matches and act accordingly

verifyDelete :: PhoneBook -> [Person] -> IO()
verifyDelete pb people = do
    if (length people == 1) then do
        -- No additional clarification required - go to the confirmation
        putStrLn "One matching entry found:"
        confirmDeletePerson pb (people!!0)

    else do
        -- Multiple matches to the search, show a numbered list and ask which to delete
        putStrLn $ "We found " ++ (show $ length people) ++ " matching entries:"
        putStrLn ""
        listNames people
        putStrLn ""
        putStrLn $ "Which would you like to delete? " ++ (show [1..(length people)])
        putStrLn "Press any other key to cancel"
        
        response <- prompt "Delete"
        
        -- If the user input matches a valid index, go to the confirm, otherwise bail out.
        -- Check the reponse string against the string values of valid indices, so as to avoid the need for string -> int parsing
        if ((length $ filter (== response) (map show [1..(length people)])) > 0) then 
            confirmDeletePerson pb (people!!((read response :: Int) - 1))
        else do
            putStrLn "No names deleted"
            putStrLn ""
        

-- IO: Confirm and execute delete

confirmDeletePerson :: PhoneBook -> Person -> IO()
confirmDeletePerson pb person = do
    putStrLn $ "Are you sure you want to delete " ++ (name person) ++ "? [y/N]"

    response <- prompt "Delete"
    
    if (response == "y") then do
        savePhoneBook $ filter (/= person) pb
        putStrLn "Deleted"
    else
        putStrLn "Not deleted"

    putStrLn ""
