module Test.Main where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Typeable (class Tag1, class Tag2, class Typeable, Proxy1(..), Proxy2(..), TypeRep, clog, eqTypeRep, typeRep, typerepImpl1, typerepImpl2)

main :: Effect Unit
main = do
  clog (eqTypeRep (typeRep :: _ (Array Person)) typeArrPerson)
  clog (eqTypeRep (typeRep :: _ (Array Person2)) typeArrPerson)
  clog (eqTypeRep (typeRep :: _ (Optional Int)) (typeRepFromVal (Some 1)))
  clog (eqTypeRep (typeRep :: _ (Optional Person)) (typeRepFromVal (Some 1)))
  clog (eqTypeRep (typeRep :: _ (Either Int Person)) (typeRep :: _ (Either Int Person)))
  clog (typeRep :: _ (Either (Either Int Int) (Optional (Array (Person)))))
  where
    typeArrPerson :: TypeRep (Array Person)
    typeArrPerson = typeRep


newtype Person = Person { name :: String, location :: String }
newtype Person2 = Person2 { name :: String, location :: String }

-- Create Typeable instances for Person
instance tag1Person :: Tag1 Person where t1 = Proxy1
instance typeablePerson :: Typeable Person where typeRep = typerepImpl1

-- Create Typeable instances for Person2
instance tag1Person2 :: Tag1 Person2 where t1 = Proxy1
instance typeablePerson2 :: Typeable Person2 where typeRep = typerepImpl1

data Optional a = None | Some a

-- Create Typeable instances for Person
instance tag2Optional :: Tag2 Optional where t2 = Proxy2
instance typeableOptional :: Typeable a => Typeable (Optional a) where
  typeRep = typerepImpl2

typeRepFromVal :: forall a. Typeable a => a -> TypeRep a
typeRepFromVal _ = typeRep
