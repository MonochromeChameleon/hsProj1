module PersonDeleter where

import DataStructure
import XmlWriter
import SearchHandler

doDelete :: PhoneBook -> IO()
doDelete pb = do
    putStrLn "Which user do you want to delete?"
    
    putStr "Delete ~> "
    searchTerm <- getLine
    putStrLn ""
    
    let matchingPeople = findPerson pb searchTerm
    if (length matchingPeople > 0) then
        verifyDelete pb matchingPeople    
    else
        putStrLn "Nobody matches that name"
        
verifyDelete :: PhoneBook -> [Person] -> IO()
verifyDelete pb people = do
    if (length people == 1) then do
        putStrLn "One matching entry found:"
        verifyDeletePerson pb (people!!0)
    else do
        putStrLn $ "We found " ++ (show $ length people) ++ " matching entries:"
        putStrLn ""
        showNames 0 people
        putStrLn ""
        putStrLn $ "Which would you like to delete? " ++ (show [1..(length people)])
        putStrLn "Press any other key to cancel"
        putStr "Delete ~> "
        response <- getLine
        putStrLn ""
        
        if ((length $ filter (== response) (map show [1..(length people)])) > 0) then 
            verifyDeletePerson pb (people!!((read response :: Int) - 1))
        else do
            putStrLn "No names deleted"
            putStrLn ""
        
verifyDeletePerson :: PhoneBook -> Person -> IO()
verifyDeletePerson pb person = do
    putStrLn $ "Are you sure you want to delete " ++ (name person) ++ "? [y/N]"

    putStr "Delete ~> "        
    response <- getLine
    putStrLn ""
    
    if (response == "y") then do
        savePhoneBook $ filter (/= person) pb
        putStrLn "Deleted"
    else
        putStrLn "Not deleted"
        
showNames :: Integer -> PhoneBook -> IO ()
showNames _ [] = putStr ""
showNames ix (person:people) = do
    putStrLn $ (show (ix + 1)) ++ ": " ++ (name person)
    showNames (ix + 1) people
