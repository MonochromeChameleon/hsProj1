-- Handles the non-persistent (in-memory) manipulation of the phone book
module CRUDUtils (addPerson, updatePersonName, addPersonPhone, deletePersonPhone, updatePersonAddress, updatePersonDoB, deletePerson) where

import DataStructure
import Utilities


--------------------------------
-- Publicly accesible methods --
--------------------------------

-- All these methods return a tuple of the edited person and the entire phone book. This is so
-- that we can continue editing the same person, within the scope of the updated phone book.

addPerson :: PhoneBook -> Name -> (Person, PhoneBook)
addPerson phoneBook name = (newPerson, newPhoneBook)
    where newPerson = createPerson name
          newPhoneBook = newPerson:phoneBook


updatePersonName :: PhoneBook -> Person -> Name -> (Person, PhoneBook)
updatePersonName phoneBook person name = (newPerson, newPhoneBook)
    where newPerson = editName person name
          newPhoneBook = replacePerson phoneBook person newPerson


addPersonPhone :: PhoneBook -> Person -> Phone -> (Person, PhoneBook)
addPersonPhone phoneBook person phone = (newPerson, newPhoneBook)
    where newPerson = addPhoneNumber person phone
          newPhoneBook = replacePerson phoneBook person newPerson
          
deletePersonPhone :: PhoneBook -> Person -> Phone -> (Person, PhoneBook)
deletePersonPhone phoneBook person phone = (newPerson, newPhoneBook)
    where newPerson = deletePhoneNumber person phone
          newPhoneBook = replacePerson phoneBook person newPerson


updatePersonAddress :: PhoneBook -> Person -> Address -> (Person, PhoneBook)
updatePersonAddress phoneBook person address = (newPerson, newPhoneBook)
    where newPerson = editAddress person address
          newPhoneBook = replacePerson phoneBook person newPerson
          
          
updatePersonDoB :: PhoneBook -> Person -> DoB -> (Person, PhoneBook)
updatePersonDoB phoneBook person date = (newPerson, newPhoneBook)
    where newPerson = editDoB person date
          newPhoneBook = replacePerson phoneBook person newPerson
          
          
deletePerson :: PhoneBook -> Person -> PhoneBook
deletePerson phoneBook person = filter (/= person) phoneBook


------------------------
-- Non-public methods --
------------------------

createPerson :: Name -> Person
-- Create a new person with the given name
createPerson name = Person name [] [] ""


editName :: Person -> Name -> Person
-- Update the name on the specified person and return the updated person
editName person name = Person name (phones person) (address person) (dob person)
    

editPhones :: Person -> Phones -> Person
-- Update the phone list on the specified person and return the updated person
editPhones person phones = Person (name person) (sortPhones phones) (address person) (dob person)
    

addPhoneNumber :: Person -> Phone -> Person
-- Add a phone number of the given type (or replace the existing one if there's a name match) and return the updated person
addPhoneNumber person phone = editPhones person newPhones
    where newPhones = phone:(filter (\x -> (fst x) /= (fst phone)) (phones person))
    

deletePhoneNumber :: Person -> Phone -> Person
-- Remove the given phone number from the person's list of phones
deletePhoneNumber person phone = editPhones person newPhones
    where newPhones = filter (\x -> x /= phone) (phones person)

editAddress :: Person -> Address -> Person
-- Update the address on the specified person and return the updated person
editAddress person address = Person (name person) (phones person) address (dob person)
    

editDoB :: Person -> DoB -> Person
-- Update the DoB on the specified person and return the updated person
editDoB person date = Person (name person) (phones person) (address person) date


replacePerson :: PhoneBook -> Person -> Person -> PhoneBook
-- Replace the specified person in the phone book and return the updated phone book
replacePerson phoneBook person replacement = replacement:(deletePerson phoneBook person)

