module Queries where

import DataStructure

inPhoneBook :: PhoneBook -> Name -> Bool
inPhoneBook pb nm = length (filter (== nm) (map name pb)) > 0