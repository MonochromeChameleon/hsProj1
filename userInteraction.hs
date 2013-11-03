module UserInteraction where

import DataStructure
import SearchHandler
import XmlWriter
import PrettyDisplay
import PersonDeleter
import PersonAdder
import PersonEditor

showHelp :: IO()
showHelp = do
    putStrLn ""
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