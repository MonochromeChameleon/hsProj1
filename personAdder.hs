module PersonAdder where

import DataStructure
import PersonEditor
import UserInteraction


-- IO: Add a new person to the phone book.

doAdd :: PhoneBook -> IO()
doAdd pb = do
    putStrLn "Who would you like to add? (or press enter to cancel)"
    
    newName <- prompt "Add"
    
    -- If a name is provided, create a Person record, and then defer through to the edit process to add more information.
    if (length newName > 0) then do
        let newPerson = Person {
            name = newName,
            phones = [],
            address = [],
            dob = ""
        }
        
        editPerson (newPerson:pb) newPerson
    else 
        putStrLn "No new entries added"
