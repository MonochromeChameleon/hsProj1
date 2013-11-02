module XmlWriter where

import DataStructure

-- Utility method - takes the tag name and content as a string and returns a full XML tag, provided there is content to include.
xout :: String -> String -> String
xout t s = if s == "" then "" else "<" ++ t ++ ">" ++ s ++ "</" ++ t ++ ">"

-- Methods for outputting individual properties

xoutName :: Person -> String
xoutName p = xout "name" (name p)

xoutPhone :: Phone -> String
xoutPhone (t, n) = xout t n

xoutPhones :: Person -> String
xoutPhones p = xout "phones" $ concat (map xoutPhone (phones p))

xoutAddressLine :: AddressLine -> String
xoutAddressLine (t,l) = xout t l

xoutAddress :: Person -> String
xoutAddress p = xout "address" $ concat (map xoutAddressLine (address p))

xoutDoB :: Person -> String
xoutDoB p = xout "dob" (dob p)

-- Output a whole person

writePerson :: Person -> String
writePerson p = xout "person" $ (xoutName p) ++ (xoutPhones p) ++ (xoutAddress p) ++ (xoutDoB p)

-- Output all entries

writePhoneBook :: PhoneBook -> String
writePhoneBook ps = concat (map writePerson ps)