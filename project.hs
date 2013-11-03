import DataStructure
import DataParser
import XmlWriter
import UserInteraction
import Control.Monad

main = do 
    showHelp -- Display help text on startup, then go into the main loop.
    getCommand
    
getCommand = do
    xs <- readFile "phoneBook.xml"
    let pb = readPhoneBook xs

    putStr "PhoneBook ~> "

    line <- getLine
    when (not $ null line) $ do
        executeCommand pb line
        getCommand