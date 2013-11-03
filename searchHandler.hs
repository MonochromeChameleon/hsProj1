module SearchHandler where

import DataStructure
import PrettyDisplay

doSearch :: PhoneBook -> IO()
doSearch pb = do
    putStrLn "Who do you want to search for?"

    putStr "Search ~> "
    searchTerm <- getLine
    putStrLn ""
    
    if (inPhoneBook pb searchTerm) then
        showPhoneBook $ findPerson pb searchTerm
    else
        putStrLn "Not found"

-- Initial substring search - returns all matching entries
findPerson :: PhoneBook -> Name -> [Person]
findPerson pb nm = (filter (\x -> take (length nm) (name x) == nm) pb)

-- Returns whether any matches exist for the given initial substring
inPhoneBook :: PhoneBook -> Name -> Bool
inPhoneBook pb nm = length (filter (\x -> take (length nm) x == nm) (map name pb)) > 0

