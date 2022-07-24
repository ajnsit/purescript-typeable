module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either)
import Data.Typeable (class TagT, TypeRep, eqTypeRep, proxyT, typeRep, typeRepFromVal)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

foreign import clog :: forall a. a -> Effect Unit

assert :: forall m. MonadThrow Error m => Boolean -> m Unit
assert = shouldEqual true
deny :: forall m. MonadThrow Error m => Boolean -> m Unit
deny = shouldEqual false

main :: Effect Unit
main = do
  launchAff_ $ runSpec [consoleReporter] do
    describe "Typeable" do
      it "can handle primitives" do
        deny $ eqTypeRep (typeRep :: _ Int) (typeRep :: _ Char)
      it "can handle unsaturated types" do
        assert $ eqTypeRep (typeRep :: _ Array) (typeRep :: _ Array)
      it "can handle mixed arity" do
        deny $ eqTypeRep (typeRep :: _ Array) (typeRep :: _ Person)
      it "can handle user defined data types" do
        assert $ eqTypeRep (typeRep :: _ Person) typePerson
        assert $ eqTypeRep (typeRep :: _ Optional) (typeRep :: _ Optional)
        deny $ eqTypeRep (typeRep :: _ (Optional Int)) (typeRep :: _ (Optional Char))
      it "can distinguish between distinct types with matching fields" do
        deny $ eqTypeRep (typeRep :: _ Person) (typeRep :: _ Person2)
      it "can handle nested types" do
        assert $ eqTypeRep (typeRep :: _ (Either Int Person)) (typeRep :: _ (Either Int Person))
        assert $ eqTypeRep (typeRep :: _ (Array Person)) typeArrPerson
        deny $ eqTypeRep (typeRep :: _ (Array Person2)) typeArrPerson
      it "can handle bare records" do
        assert $ eqTypeRep typeRecord (typeRep :: _ {name::String, age::Int})
      it "can generate type reps from values" do
        assert $ eqTypeRep (typeRep :: _ (Optional Int)) (typeRepFromVal (Some 1))
        deny $ eqTypeRep (typeRep :: _ (Optional Person)) (typeRepFromVal (Some 1))


  -- clog (eqTypeRep (typeRep :: _ Int) (typeRep :: _ Char))
  -- clog (eqTypeRep (typeRep :: _ Int) (typeRep :: _ Int))
  -- clog (typeRep :: _ Char)
  -- clog (typeRep :: _ Int)

  -- clog (typeRep :: _ Array)
  -- clog (typeRep :: _ {name::String, age::Int})
  -- clog (typeRep :: _ (Int -> Either (Either Int Int) (Optional (Array (Person)))))
  -- clog (typeRep :: _ (Either (Either Int Int) (Optional (Array (Person)))))
  -- clog (typeRep :: _ (Either Int Int))
  -- clog (typeRep :: _ (Foo Int Int Int))

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
instance taggedFoo :: TagT Foo where
  tagTDict = { tagT: proxyT }

newtype Person = Person { name :: String, location :: String }
instance TagT Person where tagTDict = { tagT: proxyT }

newtype Person2 = Person2 { name :: String, location :: String }
instance TagT Person2 where tagTDict = { tagT: proxyT }

data Optional a = None | Some a
instance TagT Optional where tagTDict = { tagT: proxyT }
