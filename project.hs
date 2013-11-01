-- Define data structure

type Name = String
type Phone = (String, String)
type Phones = [Phone]
type AddressLine = (String, String)
type Address = [AddressLine]
type DoB = String

data Person = Person { name :: Name
                     , phones :: Phones
                     , address :: Address
                     , dob :: DoB } deriving (Eq, Show)

data PhoneBook = PhoneBook [Person] deriving Show

-- /data structure

-- Define some rudimentary test data

jim = Person "Jim" [("mobile", "01234567890"), ("home", "981287673687572378")] [] "25/2/82"
bob = Person { name = "Bob", phones = [], address = [("line1", "My House"), ("line2", "My Street")], dob = "" }

pb = PhoneBook [jim, bob]

-- /test data

-- XML output functions

xout t s = if s == "" then "" else "<" ++ t ++ ">" ++ s ++ "</" ++ t ++ ">"

xoutName p = xout "name" (name p)
xoutPhone (t, n) = xout t n
xoutPhones p = xout "phones" (concat (map xoutPhone (phones p)))
xoutAddressLine (t,l) = xout t l
xoutAddress p = xout "address" (concat (map xoutAddressLine (address p)))
xoutDoB p = xout "dob" (dob p)

writePerson :: Person -> String
writePerson p = xout "person" ((xoutName p) ++ (xoutPhones p) ++ (xoutAddress p) ++ (xoutDoB p))

writePhoneBook (PhoneBook ps) = concat (map writePerson ps)

-- /XML output

-- Parsing functions

notCloseTag x = x /= '>'
notOpenTag x = x /= '<'

parseNode :: String -> (String, String)
parseNode (x:xs) = span notCloseTag xs

parseSimpleNode :: String -> ((String, String), String)
parseSimpleNode xs = ((fst node, fst value), tail $ dropWhile notCloseTag $ snd value)
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
parsePhones str = if take 8 str == "<phones>" then (fst result, tail $ dropWhile notCloseTag $ snd result) else ([], str)
    where result = parsePhonesRec (dropWhile notOpenTag $ dropWhile notCloseTag str) []

parseAddressLine :: String -> (AddressLine, String)
parseAddressLine xs = parseSimpleNode xs

parseAddressRec :: String -> [AddressLine] -> ([AddressLine], String)
parseAddressRec str ls = if take 10 str == "</address>" then (ls, str) else parseAddressRec (snd result) (ls ++ [fst result])
    where result = parseSimpleNode str

parseAddress :: String -> ([AddressLine], String)
parseAddress str = if take 9 str == "<address>" then (fst result, tail $ dropWhile notCloseTag $ snd result) else ([], str)
    where result = parseAddressRec (dropWhile notOpenTag $ dropWhile notCloseTag str) []

parseDoB :: String -> (DoB, String)
parseDoB xs = if take 5 xs == "<dob>" then (fst result, dropWhile notOpenTag $ dropWhile notCloseTag $ snd result) else ("", xs)
    where result = span notOpenTag $ tail $ snd (parseNode xs)


parsePerson :: String -> (Person, String)
parsePerson xs = (Person { name = parsedName,
                          phones = parsedPhones,
                          address = parsedAddress,
                          dob = parsedDoB }, tail $ dropWhile notCloseTag rest4)
   where personDetails = tail $ snd $ parseNode xs
         (parsedName, rest1) = parseName personDetails
         (parsedPhones, rest2) = parsePhones rest1
         (parsedAddress, rest3) = parseAddress rest2
         (parsedDoB, rest4) = parseDoB rest3


readPhoneBookRec str people = if take 8 str == "<person>" then readPhoneBookRec rest (people ++ [person]) else people
   where (person, rest) = parsePerson str

readPhoneBook :: String -> PhoneBook
readPhoneBook str = PhoneBook (readPhoneBookRec str [])


-- /Parsing

-- Cack

--isPersonRecord str = take 9 str /= "</person>"


--testSomething str = takeWhile isPersonRecord str


--splitIntoPersons str = []

--readPhoneBook str = map parsePerson $ splitIntoPersons str

main = do 
  xs <- readFile "phoneBook.xml"

  --let pb = readPhoneBook xs
  return  xs



