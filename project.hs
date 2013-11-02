import DataStructure
import DataParser
import XmlWriter
import Queries

main = do 
    xs <- readFile "phoneBook.xml"

    let pb = readPhoneBook xs
    return  pb
