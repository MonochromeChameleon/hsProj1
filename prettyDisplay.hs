-- Handles readable output of phone book entries
module PrettyDisplay (prettyPrintPhoneBook, prettyPrint) where

import DataStructure
import Utilities


-- Build up a list of strings for the entire phone book    
prettyPrintPhoneBook :: PhoneBook -> [String]
prettyPrintPhoneBook [] = delimiter
prettyPrintPhoneBook (person:people) = prettyPrint person ++ prettyPrintPhoneBook people


-- Prettify a single person's information
prettyPrint :: Person -> [String]
prettyPrint p = delimiter ++ prettyPrintName p ++ prettyPrintPhones p ++ prettyPrintAddress p ++ prettyPrintDoB p


--------------------------------------------------
-- Individual property prettification functions --
--------------------------------------------------

-- We assume that there is always a name
prettyPrintName :: Person -> [String]
prettyPrintName p = [name p, ""] -- trailing line break


-- Build a list of phone strings if any numbers are stored
prettyPrintPhones :: Person -> [String]
prettyPrintPhones p = if (count > 0) then getPhoneStrings $ phones p else []
    where count = length $ phones p

-- Recursively build phone strings as 'description: number'
getPhoneStrings :: Phones -> [String]
getPhoneStrings [] = [""] -- trailing line break
getPhoneStrings (phone:phones) = ("  " ++ (capitalize $ fst phone) ++ ": " ++ (snd phone)):getPhoneStrings phones


-- Build a list of address lines if any are stored
prettyPrintAddress :: Person -> [String]
prettyPrintAddress p = if (count > 0) then getAddressStrings $ address p else []
    where count = length $ address p

-- Recursively build an address from its component lines    
getAddressStrings :: Address -> [String]
getAddressStrings [] = [""]
getAddressStrings (line:address) = ("  " ++ snd line):getAddressStrings address


prettyPrintDoB :: Person -> [String]
prettyPrintDoB p = if (dob p /= "") then ["  DoB: " ++ dob p, ""] else []

delimiter :: [String]
delimiter = ["-------------------------", ""]
