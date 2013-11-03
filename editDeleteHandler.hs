module EditDeleteHandler where

import Data.Char

import DataStructure
import SearchHandler
import UserInteraction
import XmlWriter


-- IO: Start the edit process - prompt for a name and handle results appropriately

doEdit :: PhoneBook -> IO()
doEdit = searchPrompt "Edit" editPerson


-- IO: Confirm and execute edit

editPerson :: PhoneBook -> Person -> IO()
editPerson pb person = do
    putStrLn "No No No"


-- IO: Start delete process - prompt for a name and handle results appropriately

doDelete :: PhoneBook -> IO()
doDelete = searchPrompt "Delete" confirmDeletePerson


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
    
--------------------------------------------------------------------------------------------------------------
-- Commonised methods for the edit and delete processes - handle clarification and confirmation before passing
-- on to the appropriate callback.
--------------------------------------------------------------------------------------------------------------

-- IO: Ask who the user wants to [verb], and then pass through to the verification method

searchPrompt :: String -> (PhoneBook -> Person -> IO()) -> PhoneBook -> IO()
searchPrompt verb callback pb = do
    -- Prompt for a user search
    putStrLn $ "Who do you want to " ++ (map toLower verb) ++ "?"    

    searchTerm <- prompt verb

    let matchingPeople = findPerson pb searchTerm
    
    if (length matchingPeople > 0) then
        -- Handle the search results
        verifyPrompt verb callback pb matchingPeople
    else
        putStrLn "No matches found"
        

-- IO: Confirm which entry is to be acted upon and then pass that person back to the callback
        
verifyPrompt :: String -> (PhoneBook -> Person -> IO()) -> PhoneBook -> [Person] -> IO()
verifyPrompt verb callback pb people = do
    if (length people == 1) then do
        -- No additional clarification required - go to the confirmation
        putStrLn "One matching entry found:"
        callback pb (people!!0)

    else do
        -- Multiple matches to the search, show a numbered list and ask which to delete
        putStrLn $ "We found " ++ (show $ length people) ++ " matching entries:"
        putStrLn ""
        listNames people
        putStrLn ""
        putStrLn $ "Which would you like to " ++ (map toLower verb) ++ (show [1..(length people)])
        putStrLn "Press any other key to cancel"
        
        response <- prompt verb
        
        -- If the user input matches a valid index, go to the confirm, otherwise bail out.
        -- Check the reponse string against the string values of valid indices, so as to avoid the need for string -> int parsing
        if ((length $ filter (== response) (map show [1..(length people)])) > 0) then 
            callback pb (people!!((read response :: Int) - 1))
        else do
            putStrLn "No changes made"
            putStrLn ""


-- IO: Display a numbered list of people

listNames :: PhoneBook -> IO()
listNames = listNamesRec 0 -- Call through to the recursive function with index 0

listNamesRec :: Integer -> PhoneBook -> IO ()
listNamesRec _ [] = putStr ""
listNamesRec ix (person:people) = do
    putStrLn $ (show (ix + 1)) ++ ": " ++ (name person)
    listNamesRec (ix + 1) people
