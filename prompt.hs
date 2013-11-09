-- Custom prompt functions
module Prompt where

import DataStructure
import Utilities


-- IO: Basic prompt - prints 'Str ~>' to the console (for input str)
inlinePrompt :: String -> IO String
inlinePrompt str = do
    putStr $ (capitalize str) ++ " ~> "
    input <- getLine
    return input


-- IO: Same as the basic prompt but with a newline afterwards
prompt :: String -> IO String
prompt str = do
    input <- inlinePrompt str 
    putStrLn ""
    return input


-- IO: Prefixes the prompt with one or more lines of instructions
multilinePrompt :: String -> [String] -> IO String
multilinePrompt promptName description = do
    putLines description
    prompt promptName
    
    
-- IO: Handle several sequential prompts, returning those properties for which a value is specified as tuples paired with the prompt name
prompts :: [String] -> IO [(String, String)]
prompts [] = return []
prompts (x:xs) = do
    value <- inlinePrompt x
    otherValues <- prompts xs
    let results = if (value /= "") then (x, value):otherValues else otherValues
    return results


--------------------------------------------------------------------------------------------------------------
-- Commonised methods for the edit and delete processes - handle clarification and confirmation before passing
-- on to the appropriate callback.
--------------------------------------------------------------------------------------------------------------

-- IO: Ask who the user wants to [verb], and then pass through to the verification method
searchPrompt :: String -> ((Person, PhoneBook) -> IO()) -> PhoneBook -> IO()
searchPrompt verb callback phoneBook = do
    -- Prompt for a user search
    searchTerm <- multilinePrompt verb ["Who do you want to " ++ verb ++ "?"]

    -- Determing which entries match the search term
    let matchingPeople = findPerson phoneBook searchTerm
    
    if (length matchingPeople > 0) then
        -- Handle the matching results if there are any
        verifyPrompt verb callback phoneBook matchingPeople
    else
        -- Helpful feedback
        putStrLn "No matches found"
        

-- IO: Confirm which entry is to be acted upon and then pass that person back to the callback
verifyPrompt :: String -> ((Person, PhoneBook) -> IO()) -> PhoneBook -> [Person] -> IO()
verifyPrompt verb callback phoneBook people =
    if (length people == 1) then do
        -- No additional clarification required - go to the confirmation
        putStrLn "One matching entry found:"
        callback ((people!!0), phoneBook)

    else do
        -- Multiple matches to the search, show a numbered list and ask which to delete
        response <- multilinePrompt verb $ 
            ["We found " ++ (show $ length people) ++ " matching entries:", ""] ++ 
            listNames people ++ 
            ["", "Which would you like to " ++ verb ++ (show [1..(length people)]), "Press any other key to cancel"]
        
        -- If the user input matches a valid index, go to the confirm, otherwise bail out.
        -- Check the reponse string against the string values of valid indices, so as to avoid the need for string -> int parsing
        if ((length $ filter (== response) (map show [1..(length people)])) > 0) then 
            callback ((people!!((read response :: Int) - 1)), phoneBook)
        else do
            putLines ["No changes made", ""]
