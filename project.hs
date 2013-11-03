import Control.Monad
import DataStructure
import DataParser
import PersonAdder
import PersonDeleter
import PersonEditor
import PrettyDisplay
import SearchHandler
import UserInteraction
import XmlWriter

main = do 
    showHelp -- Display help text on startup, then go into the primary 'getCommand' loop.
    getCommand
    

-- IO: Await user command

getCommand = do
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
    case (cmd!!0) of
        's' -> doSearch pb
        'p' -> showPhoneBook pb
        'a' -> doAdd pb
        'd' -> doDelete pb
        'e' -> doEdit pb
        'x' -> displayXml pb
        'h' -> showHelp
        otherwise -> putStrLn "Unknown command"