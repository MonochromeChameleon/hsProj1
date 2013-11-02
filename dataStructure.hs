module DataStructure where
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