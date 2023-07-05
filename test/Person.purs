module Test.Person where

import Data.Typeable (class TagT, TypeRep, proxyT, typeRep)

newtype Person = Person { name :: String, location :: String }

instance tagTPerson :: TagT Person where
  tagT = proxyT

typePerson :: TypeRep Person
typePerson = typeRep

typeArrPerson :: TypeRep (Array Person)
typeArrPerson = typeRep

