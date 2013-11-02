module DataParser where

import DataStructure


notCloseTag :: Char -> Bool
notCloseTag x = x /= '>'

notOpenTag :: Char -> Bool
notOpenTag x = x /= '<'

parseNode :: String -> (String, String)
parseNode (x:xs) = span notCloseTag xs

parseSimpleNode :: String -> ((String, String), String)
parseSimpleNode xs = ((fst node, fst value), dropWhile notOpenTag $ dropWhile notCloseTag $ snd value)
    where node = parseNode xs
          value = span notOpenTag $ tail $ snd node

parseName :: String -> (Name, String)
parseName xs = if take 6 xs == "<name>" then (fst result, dropWhile notOpenTag $ dropWhile notCloseTag $ snd result) else ("", xs)
    where result = span notOpenTag $ tail $ snd (parseNode xs)

parsePhone :: String -> (Phone, String)
parsePhone xs = parseSimpleNode xs

parsePhonesRec :: String -> [Phone] -> ([Phone], String)
parsePhonesRec str ps = if take 9 str == "</phones>" then (ps, str) else parsePhonesRec (snd result) (ps ++ [fst result])
    where result = parseSimpleNode str

parsePhones :: String -> ([Phone], String)
parsePhones str = if take 8 str == "<phones>" then (fst result, dropWhile notOpenTag $ dropWhile notCloseTag $ snd result) else ([], str)
    where result = parsePhonesRec (dropWhile notOpenTag $ dropWhile notCloseTag str) []

parseAddressLine :: String -> (AddressLine, String)
parseAddressLine xs = parseSimpleNode xs

parseAddressRec :: String -> [AddressLine] -> ([AddressLine], String)
parseAddressRec str ls = if take 10 str == "</address>" then (ls, str) else parseAddressRec (snd result) (ls ++ [fst result])
    where result = parseSimpleNode str

parseAddress :: String -> ([AddressLine], String)
parseAddress str = if take 9 str == "<address>" then (fst result, dropWhile notOpenTag $ dropWhile notCloseTag $ snd result) else ([], str)
    where result = parseAddressRec (dropWhile notOpenTag $ dropWhile notCloseTag str) []

parseDoB :: String -> (DoB, String)
parseDoB xs = if take 5 xs == "<dob>" then (fst result, dropWhile notOpenTag $ dropWhile notCloseTag $ snd result) else ("", xs)
    where result = span notOpenTag $ tail $ snd (parseNode xs)


parsePerson :: String -> (Person, String)
parsePerson xs = (Person { name = parsedName,
                          phones = parsedPhones,
                          address = parsedAddress,
                          dob = parsedDoB }, dropWhile notOpenTag $ tail $ dropWhile notCloseTag rest4)
   where personDetails = dropWhile notOpenTag $ tail $ snd $ parseNode xs
         (parsedName, rest1) = parseName personDetails
         (parsedPhones, rest2) = parsePhones rest1
         (parsedAddress, rest3) = parseAddress rest2
         (parsedDoB, rest4) = parseDoB rest3


readPhoneBookRec :: String -> [Person] -> [Person]
readPhoneBookRec str people = if take 8 str == "<person>" then readPhoneBookRec rest (people ++ [person]) else people
   where (person, rest) = parsePerson str

readPhoneBook :: String -> PhoneBook
readPhoneBook str = PhoneBook (readPhoneBookRec str [])
