module Test.Main where

import Prelude

import Data.Either (Either)
import Data.Typeable (class Tag0, class Tag1, class Tag3, TypeRep, eqTypeRep, proxy0, proxy1, proxy3, typeRep, typeRepFromVal)
import Effect (Effect)

foreign import clog :: forall a. a -> Effect Unit

main :: Effect Unit
main = do
  clog (eqTypeRep (typeRep :: _ Person) typePerson)
  clog (eqTypeRep (typeRep :: _ (Array Person)) typeArrPerson)
  clog (eqTypeRep (typeRep :: _ (Array Person2)) typeArrPerson)
  clog (eqTypeRep (typeRep :: _ (Optional Int)) (typeRepFromVal (Some 1)))
  clog (eqTypeRep (typeRep :: _ (Optional Person)) (typeRepFromVal (Some 1)))
  clog (eqTypeRep (typeRep :: _ (Either Int Person)) (typeRep :: _ (Either Int Person)))
  clog (typeRep :: _ (Either (Either Int Int) (Optional (Array (Person)))))
  clog (typeRep :: _ (Either (Either Int Int) (Optional (Array (Person)))))
  clog (typeRep :: _ (Either Int Int))
  clog (typeRep :: _ Int)
  clog (typeRep :: _ (Foo Int Int Int))
  where
    typeArrPerson :: TypeRep (Array Person)
    typeArrPerson = typeRep
    typePerson :: TypeRep Person
    typePerson = typeRep

data Foo a b c = Foo
instance tag3Foo :: Tag3 Foo where tag3 = proxy3

newtype Person = Person { name :: String, location :: String }
instance tag0Person :: Tag0 Person where tag0 = proxy0

newtype Person2 = Person2 { name :: String, location :: String }
instance tag0Person2 :: Tag0 Person2 where tag0 = proxy0

data Optional a = None | Some a
instance tag1Optional :: Tag1 Optional where tag1 = proxy1
