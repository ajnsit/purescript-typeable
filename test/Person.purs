module Test.Person where

import Data.Typeable (class Tagged, TypeRep, makeTag, typeRep)
import Data.Unit (unit)

newtype Person = Person { name :: String, location :: String }

instance taggedPerson :: Tagged Person where
  tag = makeTag unit

typePerson :: TypeRep Person
typePerson = typeRep

typeArrPerson :: TypeRep (Array Person)
typeArrPerson = typeRep

