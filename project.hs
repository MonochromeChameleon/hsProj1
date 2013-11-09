import Control.Monad
import Data.Char
import System.Directory
import System.IO

import DataStructure
import DataParser
import PhoneBookIO
import PrettyDisplay
import Prompt
import Utilities
import XmlWriter

main :: IO()
main = do 
    hSetBuffering stdout NoBuffering -- Lets us have a nice custom prompt so it's easier to know what you're doing.
    showHelp                         -- Display help text on startup, then go into the primary 'getCommand' loop.
    getCommand                       -- Enter the main loop.
    

-- IO: Await user command
getCommand :: IO()
getCommand = do
    -- Use a temporary file and rename it in the run loop rather than passing file handles through the entire 
    -- system. This bypasses conflicting file access problems.
    fileExists <- doesFileExist ".phoneBook"
    when fileExists $ renameFile ".phoneBook" "phoneBook.xml"
    
    xs <- readFile "phoneBook.xml" -- Reload the phone book each time through the loop in case we have edited it
    let pb = readPhoneBook xs

    line <- prompt "PhoneBook" -- Custom prompt

    when (not $ null line) $ do
        executeCommand pb line
        getCommand -- run loop


-- IO: Display help text

showHelp :: IO()
showHelp = putLines [
    "Welcome to the Vig, Nits and Hugh phone book program",
    "",
    "Available commands:",
    "    s: Search entries",
    "    p: Print entire phone book",
    "    a: Add entry",
    "    e: Edit an entry",
    "    d: Delete entry",
    "    x: Show phone book as xml",
    "",
    "    h: Show this help",
    "",
    "Press enter to exit",
    ""]
    
    
-- IO: Execute apppropriate command given first character at the PhoneBook prompt
    
executeCommand :: PhoneBook -> String -> IO ()
executeCommand pb cmd = do
    -- Case-insensitive just to make life easier
    case (toLower $ cmd!!0) of
        's' -> doSearch pb
        'p' -> putLines $ prettyPrintPhoneBook pb
        'a' -> doAdd pb
        'e' -> doEdit pb
        'd' -> doDelete pb
        'x' -> putStrLn $ writePhoneBook pb
        'h' -> showHelp
        otherwise -> putStrLn "Unknown command"
    