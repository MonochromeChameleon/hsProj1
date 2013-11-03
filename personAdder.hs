module PersonAdder where

import DataStructure
import PersonEditor

doAdd :: PhoneBook -> IO()
doAdd pb = do
    putStrLn "Who would you like to add? (or press enter to cancel)"
    
    putStr "Add ~> "
    newName <- getLine
    
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

