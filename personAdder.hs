module PersonAdder where

import DataStructure
import EditDeleteHandler
import UserInteraction
import XmlWriter

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
        
        savePhoneBook (newPerson:pb)
        
        putStrLn $ "Added. Edit " ++ newName ++ "'s details"
        
        editPerson (newPerson:pb) newPerson
    else 
        putStrLn "No new entries added"
