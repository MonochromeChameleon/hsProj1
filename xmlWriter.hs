module XmlWriter (writePhoneBook) where

import DataStructure

-- Output all entries as XML
writePhoneBook :: PhoneBook -> String
writePhoneBook ps = concat (map writePerson ps)

-- Output a person as XML
writePerson :: Person -> String
writePerson p = xout 0 "person" $ (xoutName p) ++ (xoutPhones p) ++ (xoutAddress p) ++ (xoutDoB p) ++ "\n"

-- Methods for outputting individual properties

xoutName :: Person -> String
xoutName p = xout 1 "name" (name p)

xoutPhone :: Phone -> String
xoutPhone (t, n) = xout 2 t n

xoutPhones :: Person -> String
xoutPhones p = if (length (phones p) > 0) then xout 1 "phones" $ concat (map xoutPhone (phones p)) ++ "\n    " else ""

xoutAddressLine :: AddressLine -> String
xoutAddressLine (t,l) = xout 2 t l

xoutAddress :: Person -> String
xoutAddress p = if (length (address p) > 0) then xout 1 "address" $ concat (map xoutAddressLine (address p)) ++ "\n    " else ""

xoutDoB :: Person -> String
xoutDoB p = xout 1 "dob" (dob p)

-- Utility method - takes the tag name and content as a string and returns a full XML tag, provided there is content to include.
xout :: Int -> String -> String -> String
xout indentation tagName content = 
    if content == "" then "" 
    else "\n" ++ (replicate (indentation * 4) ' ') ++ "<" ++ tagName ++ ">" ++ content ++ "</" ++ tagName ++ ">"
