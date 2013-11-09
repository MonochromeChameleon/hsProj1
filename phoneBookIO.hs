-- Primary module for handling the IO side of CRUD operations
module PhoneBookIO (doAdd, doSearch, doEdit, doDelete) where

import Data.Char
import Data.List

import DataStructure
import CRUDUtils
import PrettyDisplay
import Prompt
import Utilities
import XmlWriter


-- IO: Write phone book back to a temporary file
savePhoneBook :: PhoneBook -> IO()
-- We write to a temporary file as this will avoid file handle conflicts. An alternative would be to pass
-- a reference to the open file around from function to function, but that seems like it would result in
-- unnecessary clutter throughout the codebase.
savePhoneBook pb = writeFile ".phoneBook" $ writePhoneBook $ sort pb


------------------------------------------------
-------------------- Create --------------------
------------------------------------------------

-- IO: Add a new person to the phone book.
doAdd :: PhoneBook -> IO()
doAdd pb = do    
    newName <- multilinePrompt "Name" ["Who would you like to add?", "(press enter to cancel and return to the main menu)"]
    
    if (length newName > 0) then do
        -- If a name is provided, create a Person, and then defer through to the edit process to
        -- populate them with more information.
        putStrLn $ "Added. Edit " ++ newName ++ "'s details"
        editPerson $ addPerson pb newName
    else
        -- Helpful feedback, then back to the main prompt.
        putStrLn "No new entries added"


------------------------------------------------
--------------------- Read ---------------------
------------------------------------------------

-- IO: Execute a search and display prettified matching results
doSearch :: PhoneBook -> IO()
doSearch pb = do
    searchTerm <- multilinePrompt "Search" ["Who do you want to search for?"]
    
    if (inPhoneBook pb searchTerm) then
        -- Display all the matching results in the console.
        putLines $ prettyPrintPhoneBook $ findPerson pb searchTerm
    else
        -- Helpful feedback
        putStrLn "Not found"


------------------------------------------------
-------------------- Update --------------------
------------------------------------------------

-- IO: Start the edit process - prompt for a name and handle results appropriately
doEdit :: PhoneBook -> IO()
doEdit = searchPrompt "edit" editPerson


-- IO: Confirm and execute edit
editPerson :: (Person, PhoneBook) -> IO()
editPerson (person, pb) = do
    putLines $ prettyPrint person -- Display the current information
    
    -- Show edit instructions and await a command
    cmd <- multilinePrompt "Edit" ["What do you want to edit?", "1: Name", "2: Phone numbers", "3: Address", "4: DoB", "Press any other key to return to the main menu"]

    case cmd of
        "1" -> doEditName pb person
        "2" -> doEditPhones pb person
        "3" -> doEditAddress pb person
        "4" -> doEditDoB pb person
        -- We only need to save when we're leaving this method, as we will be passing references around the rest of the time
        otherwise -> savePhoneBook pb 
        

-- IO: update the given person's name in the phone book and call back to editPerson
doEditName :: PhoneBook -> Person -> IO()
doEditName pb person = do
    -- Get the updated name
    nm <- prompt "Name"
    
    -- Call through to the editPerson method with the updated person and phone book
    editPerson $ updatePersonName pb person nm
    
    
-- IO: ask whether the user wants to add, update or delete a phone number
doEditPhones :: PhoneBook -> Person -> IO()
doEditPhones phoneBook person = do
    cmd <- multilinePrompt "Phone" ["Do you want to add, edit or delete a phone number?", "1: Add", "2: Edit", "3: Delete", "Press any other key to return to the edit menu"]
    
    case (toLower $ cmd!!0) of
        '1' -> doAddPhone phoneBook person
        'a' -> doAddPhone phoneBook person
        '2' -> doEditPhone phoneBook person
        'e' -> doEditPhone phoneBook person
        '3' -> doDeletePhone phoneBook person
        'd' -> doDeletePhone phoneBook person
        otherwise -> editPerson (person, phoneBook) -- Go back out one level if they don't choose a valid option


-- IO: update the given person's phones in the phone book and call back to editPerson
doAddPhone :: PhoneBook -> Person -> IO()
doAddPhone pb person = do
    -- Prompt for the phone type and number
    phoneType <- inlinePrompt "Phone Type"
    phoneNumber <- prompt $ (capitalize phoneType) ++ " Number"
    
    -- Call through to the editPerson method with the updated person and phone book
    editPerson $ addPersonPhone pb person (map toLower phoneType, phoneNumber)
    
    
-- IO: edit an existing phone number
doEditPhone :: PhoneBook -> Person -> IO()
doEditPhone phoneBook person = do
    cmd <- multilinePrompt "Phone" $ ["Which phone number do you want to edit?"] ++ listPhones person
    
    -- If the user input matches a valid index, go to the confirm, otherwise bail out.
    -- Check the reponse string against the string values of valid indices, so as to avoid the need for string -> int parsing
    if ((length $ filter (== cmd) (map show [1..(length (phones person))])) > 0) then do
        let phone = (phones person)!!((read cmd :: Int) - 1)
        phoneNumber <- prompt $ (capitalize $ fst phone) ++ " Number"
        
        -- Call through to the editPerson method with the updated person and phone book
        editPerson $ addPersonPhone phoneBook person (fst phone, phoneNumber)
    else
        -- Invalid choice, so go back up to the edit menu
        editPerson (person, phoneBook)


-- IO: delete an existing phone number
doDeletePhone :: PhoneBook -> Person -> IO()
doDeletePhone phoneBook person = do
    cmd <- multilinePrompt "Phone" $ ["Which phone number do you want to delete?"] ++ listPhones person
    
    -- If the user input matches a valid index, go to the confirm, otherwise bail out.
    -- Check the reponse string against the string values of valid indices, so as to avoid the need for string -> int parsing
    if ((length $ filter (== cmd) (map show [1..(length (phones person))])) > 0) then do
        let phone = (phones person)!!((read cmd :: Int) - 1)

        response <- multilinePrompt "Delete" ["Are you sure you want to delete " ++ (name person) ++ "'s " ++ (fst phone) ++ " number? [y/N]"]
        
        if response == "y" then
            editPerson $ deletePersonPhone phoneBook person phone
        else
            -- Invalid choice, so go back up to the edit menu
            editPerson (person, phoneBook)
    else
        -- Invalid choice, so go back up to the edit menu
        editPerson (person, phoneBook)

-- IO: update the given person's address in the phone book and call back to editPerson
doEditAddress :: PhoneBook -> Person -> IO()
doEditAddress pb person = do
    -- Prompt for the various address components in order
    newAddress <- prompts ["line 1", "line 2", "postcode", "city"]
    
    -- Call through to the editPerson method with the updated person and phone book
    editPerson $ updatePersonAddress pb person newAddress


-- IO: update the given person's DoB in the phone book and call back to editPerson
doEditDoB :: PhoneBook -> Person -> IO()
doEditDoB pb person = do
    -- Prompt for a DoB
    dte <- prompt "DoB"

    -- Call through to the editPerson method with the updated person and phone book
    editPerson $ updatePersonDoB pb person dte
    

------------------------------------------------
-------------------- Delete --------------------
------------------------------------------------

-- IO: Start delete process - prompt for a name and handle results appropriately
doDelete :: PhoneBook -> IO()
doDelete = searchPrompt "delete" confirmDeletePerson


-- IO: Confirm and execute delete
confirmDeletePerson :: (Person, PhoneBook) -> IO()
confirmDeletePerson (person, pb) = do

    response <- multilinePrompt "Delete" ["Are you sure you want to delete " ++ (name person) ++ "? [y/N]"]
    
    if (response == "y") then do
        savePhoneBook $ deletePerson pb person
        putStrLn "Deleted"
    else
        putStrLn "Not deleted"
