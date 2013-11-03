module EditDeleteHandler where

import Data.Char

import DataStructure
import PrettyDisplay
import SearchHandler
import UserInteraction
import XmlWriter


-- IO: Start the edit process - prompt for a name and handle results appropriately

doEdit :: PhoneBook -> IO()
doEdit = searchPrompt "Edit" editPerson


-- IO: Confirm and execute edit

editPerson :: PhoneBook -> Person -> IO()
editPerson pb person = do
    showPerson person

    putStrLn "What do you want to edit?"
    putStrLn "1: Name"
    putStrLn "2: Phone numbers"
    putStrLn "3: Address"
    putStrLn "4: DoB"

    cmd <- prompt "Edit"
    
    case cmd of
        "1" -> editName pb person
        "2" -> editPhones pb person
        "3" -> editAddress pb person
        "4" -> editDoB pb person
        otherwise -> putStr ""
        
editName :: PhoneBook -> Person -> IO()
editName pb person = do
    nm <- prompt "Name"
    
    let newPerson = Person {
        name = nm,
        phones = (phones person),
        address = (address person),
        dob = (dob person)
    }
    
    let newPhoneBook = newPerson:(filter (/= person) pb)
    savePhoneBook newPhoneBook
    
    editPerson newPhoneBook newPerson

editPhones :: PhoneBook -> Person -> IO()
editPhones pb person = do
    phoneType <- inlinePrompt "Phone Type"
    phoneNumber <- prompt $ (capitalize phoneType) ++ " Number"
    
    let phone = (phoneType, phoneNumber)
    
    let newPhoneList = phone:(filter (\x -> (fst x) /= phoneType) (phones person))
    let newPerson = Person {
        name = (name person),
        phones = newPhoneList,
        address = (address person),
        dob = (dob person)
    }
    
    let newPhoneBook = newPerson:(filter (/= person) pb)
    savePhoneBook newPhoneBook
    
    editPerson newPhoneBook newPerson


editAddress :: PhoneBook -> Person -> IO()
editAddress pb person = do
    line1 <- inlinePrompt "Line 1"
    line2 <- inlinePrompt "Line 2"
    postcode <- inlinePrompt "Postcode"
    city <- prompt "City"
    
    let newAddress = filter (\x -> snd x /= "") [("line1", line1), ("line2", line2), ("postcode", postcode), ("city", city)]
    
    let newPerson = Person {
        name = (name person),
        phones = (phones person),
        address = newAddress,
        dob = (dob person)
    }
    
    let newPhoneBook = newPerson:(filter (/= person) pb)
    savePhoneBook newPhoneBook
    
    editPerson newPhoneBook newPerson


editDoB :: PhoneBook -> Person -> IO()
editDoB pb person = do
    d <- prompt "DoB"
    
    let newPerson = Person {
        name = (name person),
        phones = (phones person),
        address = (address person),
        dob = d
    }
    
    let newPhoneBook = newPerson:(filter (/= person) pb)
    savePhoneBook newPhoneBook
    
    editPerson newPhoneBook newPerson

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
