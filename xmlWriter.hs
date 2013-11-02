module XmlWriter where

import DataStructure

xout :: String -> String -> String
xout t s = if s == "" then "" else "<" ++ t ++ ">" ++ s ++ "</" ++ t ++ ">"

xoutName :: Person -> String
xoutName p = xout "name" (name p)

xoutPhone :: (String, String) -> String
xoutPhone (t, n) = xout t n

xoutPhones :: Person -> String
xoutPhones p = xout "phones" (concat (map xoutPhone (phones p)))

xoutAddressLine :: (String, String) -> String
xoutAddressLine (t,l) = xout t l

xoutAddress :: Person -> String
xoutAddress p = xout "address" (concat (map xoutAddressLine (address p)))

xoutDoB :: Person -> String
xoutDoB p = xout "dob" (dob p)

writePerson :: Person -> String
writePerson p = xout "person" ((xoutName p) ++ (xoutPhones p) ++ (xoutAddress p) ++ (xoutDoB p))

writePhoneBook :: PhoneBook -> String
writePhoneBook (PhoneBook ps) = concat (map writePerson ps)