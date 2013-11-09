-- Defines the data types in our phone book
module DataStructure where

import Data.Char

-- PhoneBook is simply a collection of Person records
type PhoneBook = [Person]


-------------------------------
-- Individual Person records --
-------------------------------

data Person = Person { name :: Name
                     , phones :: Phones
                     , address :: Address
                     , dob :: DoB } deriving (Eq, Show)
                     
-- Define ordering on Person records so that we can keep the phone book in alphabetical order.
instance Ord Person where
    (<)  p1 p2 = (<)  (map toLower $ name p1) (map toLower $ name p2)
    (<=) p1 p2 = (<=) (map toLower $ name p1) (map toLower $ name p2)
    (>)  p1 p2 = (>)  (map toLower $ name p1) (map toLower $ name p2)
    (>=) p1 p2 = (>=) (map toLower $ name p1) (map toLower $ name p2)
    

----------------------------------
-- Individual property synonyms --
----------------------------------

type Name = String

type Phone = (String, String)
type Phones = [Phone]

type AddressLine = (String, String)
type Address = [AddressLine]

type DoB = String
