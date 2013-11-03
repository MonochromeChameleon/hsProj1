import Control.Monad
import Data.Char
import System.Directory
import System.IO

import DataStructure
import DataParser
import EditDeleteHandler
import PersonAdder
import PrettyDisplay
import SearchHandler
import UserInteraction
import XmlWriter

main :: IO()
main = do 
    hSetBuffering stdout NoBuffering -- Lets us have a nice custom prompt so it's easier to know what you're doing.
    showHelp                         -- Display help text on startup, then go into the primary 'getCommand' loop.
    getCommand                       -- Enter the main loop.
    

-- IO: Await user command
getCommand :: IO()
getCommand = do
    -- If we have written to a temp file, we want to overwrite the actual file (this is to get around conflicting file handles)
    fileExists <- doesFileExist ".phoneBook"
    if (fileExists) then
        renameFile ".phoneBook" "phoneBook.xml"
    else 
        putStr "" -- no-op in the else block
    
    xs <- readFile "phoneBook.xml" -- Reload the phone book each time through the loop in case we have edited it
    let pb = readPhoneBook xs

    line <- prompt "PhoneBook" -- Custom prompt

    when (not $ null line) $ do
        executeCommand pb line
        getCommand


-- IO: Display help text

showHelp :: IO()
showHelp = do
    putStrLn "Welcome to the Vig, Nits and Hugh phone book program"
    putStrLn ""
    putStrLn "Available commands:"
    putStrLn "    s: Search entries"
    putStrLn "    p: Print entire phone book"
    putStrLn "    a: Add entry"
    putStrLn "    d: Delete entry"
    putStrLn "    e: Edit an entry"
    putStrLn "    x: Show phone book as xml"
    putStrLn "    h: Show this help"
    putStrLn ""
    putStrLn "Press enter to exit"
    putStrLn ""
    
    
-- IO: Execute apppropriate command given first character at the PhoneBook prompt
    
executeCommand :: PhoneBook -> String -> IO ()
executeCommand pb cmd = do
    -- Case-insensitive just to make life easier
    case (toLower $ cmd!!0) of
        's' -> doSearch pb
        'p' -> showPhoneBook pb
        'a' -> doAdd pb
        'd' -> doDelete pb
        'e' -> doEdit pb
        'x' -> displayXml pb
        'h' -> showHelp
        otherwise -> putStrLn "Unknown command"