module Test.Main where

import Prelude

import Data.Either (Either)
import Data.Typeable (class TagT, TypeRep, eqTypeRep, proxyT, typeRep, typeRepFromVal)
import Effect (Effect)

foreign import clog :: forall a. a -> Effect Unit

main :: Effect Unit
main = do
  clog (eqTypeRep (typeRep :: _ Int) (typeRep :: _ Char))
  clog (eqTypeRep (typeRep :: _ Int) (typeRep :: _ Int))
  clog (typeRep :: _ Char)
  clog (typeRep :: _ Int)

  clog (typeRep :: _ Array)
  clog (typeRep :: _ {name::String, age::Int})
  clog (typeRep :: _ (Int -> Either (Either Int Int) (Optional (Array (Person)))))
  clog (typeRep :: _ (Either (Either Int Int) (Optional (Array (Person)))))
  clog (typeRep :: _ (Either Int Int))
  clog (typeRep :: _ (Foo Int Int Int))

  clog (eqTypeRep (typeRep :: _ Array) (typeRep :: _ Array))
  clog (eqTypeRep (typeRep :: _ Person) typePerson)
  clog (eqTypeRep (typeRep :: _ (Array Person)) typeArrPerson)
  clog (eqTypeRep (typeRep :: _ (Array Person2)) typeArrPerson)
  clog (eqTypeRep typeRecord (typeRep :: _ {name::String, age::Int}))
  clog (eqTypeRep (typeRep :: _ (Optional Int)) (typeRepFromVal (Some 1)))
  clog (eqTypeRep (typeRep :: _ (Optional Person)) (typeRepFromVal (Some 1)))
  clog (eqTypeRep (typeRep :: _ (Either Int Person)) (typeRep :: _ (Either Int Person)))

  where
    typeRecord :: TypeRep {age::Int, name::String}
    typeRecord = typeRep
    typeArrPerson :: TypeRep (Array Person)
    typeArrPerson = typeRep
    typePerson :: TypeRep Person
    typePerson = typeRep
    -- The following should not compile since Break does not have a typeable instance
    -- typeRecordBreak :: TypeRep {break::Break, name::String}
    -- typeRecordBreak = typeRep

-- A data type without a typeable instance
data Break

data Foo :: forall k1 k2 k3. k1 -> k2 -> k3 -> Type
data Foo a b c = Foo
instance tagFoo :: TagT Foo where tagT = proxyT

newtype Person = Person { name :: String, location :: String }
instance tagTPerson :: TagT Person where tagT = proxyT

newtype Person2 = Person2 { name :: String, location :: String }
instance tagTPerson2 :: TagT Person2 where tagT = proxyT

data Optional a = None | Some a
instance tagOptional :: TagT Optional where tagT = proxyT
