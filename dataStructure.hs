module DataStructure where

-- Individual property synonyms

type Name = String

type Phone = (String, String)
type Phones = [Phone]

type AddressLine = (String, String)
type Address = [AddressLine]

type DoB = String

-- Individual Person records

data Person = Person { name :: Name
                     , phones :: Phones
                     , address :: Address
                     , dob :: DoB } deriving (Eq, Show)
                     
instance Ord Person where
    (<) p1 p2 = (<) (name p1) (name p2)
    (<=) p1 p2 = (<=) (name p1) (name p2)
    (>) p1 p2 = (>) (name p1) (name p2)
    (>=) p1 p2 = (>=) (name p1) (name p2)

-- PhoneBook is simply a collection of Person records

type PhoneBook = [Person]