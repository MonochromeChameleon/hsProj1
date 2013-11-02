import DataStructure
import DataParser
import XmlWriter

main = do 
  xs <- readFile "phoneBook.xml"
  	
  let pb = readPhoneBook xs
  return  pb
