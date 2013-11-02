module DataParser where

import DataStructure


-- Parsing functions

readPhoneBook :: String -> PhoneBook
readPhoneBook = readPhoneBookRec [] -- Defer through to the recursive function with an empty starting array

readPhoneBookRec :: [Person] -> String -> [Person]
readPhoneBookRec people str = if checkTagInner "person" str -- Bail out if our string doesn't start with <person> (i.e. end of file)
    then readPhoneBookRec (person:people) rest
    else reverse people -- maintain order
    where (person, rest) = parsePerson str


parsePerson :: String -> (Person, String)
parsePerson str = (Person {
        name    = parsedName,
        phones  = parsedPhones,
        address = parsedAddress,
        dob     = parsedDoB
    }, nextXmlTag rest4)
                          
   where personDetails = nextXmlTag str -- Trim off the '<person>...' up to '<name>'
         (parsedName,    rest1) = parseName    personDetails
         (parsedPhones,  rest2) = parsePhones  rest1
         (parsedAddress, rest3) = parseAddress rest2
         (parsedDoB,     rest4) = parseDoB     rest3


-- Simple parsing methods

parseName :: String -> (Name, String)
parseName = parseSimpleNodeContent "name"

parseDoB :: String -> (DoB, String)
parseDoB = parseSimpleNodeContent "dob"

parseSimpleNodeContent :: String -> String -> (String, String)
parseSimpleNodeContent nodeName str = if checkTagInner nodeName str -- Check that we are at the expected node
    then getNodeContent str                                         -- Parse if correct
    else ("", str)                                                  -- Otherwise skip straight on


-- More involved parsing methods

parsePhones :: String -> (Phones, String)
parsePhones = parseNestedNodeContent "phones"

parseAddress :: String -> (Address, String)
parseAddress = parseNestedNodeContent "address"


parseNestedNodeContent :: String -> String -> ([(String, String)], String)
parseNestedNodeContent nodeName str = if checkTagInner nodeName str -- Only try to parse if we have the right tag
    then parseNestedNodesRec nodeName [] (nextXmlTag str)           -- Defer through to the recursive parser
    else ([], str)                                                  -- Or return an empty list 
    
-- Recursively collect (node name, node content) pairs
parseNestedNodesRec :: String -> [(String, String)] -> String -> ([(String, String)], String)
parseNestedNodesRec nodeName parsedNodes str = if checkTagInner ('/':nodeName) str  -- Bail out of parsing when we reach the end of our section
    then (reverse parsedNodes, nextXmlTag str)
    else parseNestedNodesRec nodeName (fst result:parsedNodes) (snd result)
    where result = parseNode str


-- Utility functions for scanning and reading xml tags

notCloseTag :: Char -> Bool
notCloseTag x = x /= '>'

notOpenTag :: Char -> Bool
notOpenTag x = x /= '<'

-- Removes an xml tag and everything up to the next XML tag from the front of a string
nextXmlTag :: String -> String
nextXmlTag str = dropWhile notOpenTag $ dropWhile notCloseTag str

-- Get the name of the next xml node
getNodeName :: String -> (String, String)
getNodeName str = span notCloseTag $ tail $ dropWhile notOpenTag str

-- Get the content of the next xml tag
getNodeContent :: String -> (String, String)
getNodeContent str = (fst result, nextXmlTag $ snd result)
    where result = span notOpenTag $ tail $ dropWhile notCloseTag str

-- Return the name and content of the next xml node
parseNode :: String -> ((String, String), String)
parseNode str = ((fst node, fst value), snd value)
    where node = getNodeName str
          value = getNodeContent str

checkTagInner :: String -> String -> Bool
checkTagInner expected str = (length str > 0) && found == expected
    where found = fst $ getNodeName str