type Name = String
type Phone = (String, String)
type Phones = [Phone]
type AddressPart = (String, String)
type Address = [AddressPart]
type DoB = (Int, Int, Int)

data Property = Name | Phones | Address | DoB 

data Person = Person Name Phones Address DoB deriving Show
data PhoneBook = PhoneBook [Person]

jim = Person "Jim" [("mobile", "01234567890")] [] (25,2,82)
bob = Person "Bob" [] [("line1", "My House"), ("line2", "My Street")] (0,0,0)

pb = PhoneBook [jim, bob]

getName (Person n _ _ _) = n

getPhones (Person _ p _ _) = p

getAddress (Person _ _ a _) = a

getDoB (Person _ _ _ d) = d
writeDoB (dd, mm, yy) = if dd > 0 && mm > 0 && yy > 0 then (show dd) ++ "/" ++ (show mm) ++ "/" ++ (show yy) else ""

{-get :: Person -> String -> Property
get (Person n p a d) prop = case prop!!0 of
  'n' -> n
  'p' -> p
  'a' -> a
  'd' -> d
  otherwise -> error "undefined property"
-}
xout t s = if s == "" then "" else "<" ++ t ++ ">" ++ s ++ "</" ++ t ++ ">"

xoutName p = xout "name" (getName p)
xoutPhone (t, n) = xout t n
xoutPhones p = xout "phones" (concat (map xoutPhone (getPhones p)))
xoutAddressLine (t,l) = xout t l
xoutAddress p = xout "address" (concat (map xoutAddressLine (getAddress p)))
xoutDoB p = xout "dob" (writeDoB(getDoB p))

writePerson :: Person -> String
writePerson p = xout "person" ((xoutName p) ++ (xoutPhones p) ++ (xoutAddress p) ++ (xoutDoB p))

writePhoneBook (PhoneBook ps) = concat (map writePerson ps)
