module Data.Typeable
  ( TypeRep
  , class Typeable
  , class TypeableRecordFields
  , typeableRecordFields
  , TypeRow
  , typeRep
  , eqT
  , eqTypeRep
  , cast
  , gcast
  , gcast1
  , gcast2
  , typeRepFromVal
  , SomeTypeRep(..)
  , wrapSomeTypeRep
  , unwrapSomeTypeRep
  , runSomeTypeRep
  , eqSomeTypeRep
  , typeRepFromTag
  , Tag
  , makeTag
  , class Tagged
  , tag
  ) where

import Control.Category (identity)
import Data.Boolean (otherwise)
import Data.Either (Either)
import Data.Function ((#))
import Data.Functor (map)
import Data.Identity (Identity(..))
import Data.Leibniz (type (~), Leibniz, runLeibniz)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ordering (Ordering)
import Data.Show (class Show)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit, unit)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Indexed TypeReps
data TypeRep :: forall k. k -> Type
data TypeRep a

-- | `Typeable` things have a `TypeRep`
class Typeable :: forall k. k -> Constraint
class Typeable a where
  typeRep :: TypeRep a

instance showTypeRepInstance :: Show (TypeRep a) where
  show t = showTypeRep t

-- | Compare two `TypeRep`, and potentially return a type witness for equality
eqT :: forall a b. TypeRep a -> TypeRep b -> Maybe (a ~ b)
eqT ta tb
  | eqTypeRep ta tb = Just (unsafeCoerce (identity :: Leibniz a a))
  | otherwise = Nothing

cast :: forall a b. Typeable a => Typeable b => a -> Maybe b
cast a = map unwrap (gcast (Identity a))

gcast :: forall m a b. Typeable a => Typeable b => m a -> Maybe (m b)
gcast a = m # map \f -> runLeibniz f a
  where
  m = eqT (typeRep :: _ a) (typeRep :: _ b)

gcast1 :: forall s t a c. Typeable (s a) => Typeable (t a) => c (s a) -> Maybe (c (t a))
gcast1 a = m # map \f -> runLeibniz f a
  where
  m = eqT (typeRep :: _ (s a)) (typeRep :: _ (t a))

gcast2 :: forall s t a b c. Typeable (s a b) => Typeable (t a b) => c (s a b) -> Maybe (c (t a b))
gcast2 a = m # map \f -> runLeibniz f a
  where
  m = eqT (typeRep :: _ (s a b)) (typeRep :: _ (t a b))

-- | Compare two `TypeRep`, potentially of differing types, for equality
foreign import eqTypeRep :: forall a b. TypeRep a -> TypeRep b -> Boolean

-- | Get the `TypeRep` for a value
typeRepFromVal :: forall a. Typeable a => a -> TypeRep a
typeRepFromVal _ = typeRep

-- | Unindexed typereps
-- | Note: Can't use the `exists` package because it doesn't gel with the polykinded TypeRep
data SomeTypeRep

mkSomeTypeRep :: forall a. TypeRep a -> SomeTypeRep
mkSomeTypeRep = unsafeCoerce

-- | Run a function on a TypeRep from a SomeTypeRep
runSomeTypeRep :: forall r. (forall a. TypeRep a -> r) -> SomeTypeRep -> r
runSomeTypeRep = unsafeCoerce

-- | Wrap a TypeRep into a SomeTypeRep
wrapSomeTypeRep :: forall a. TypeRep a -> SomeTypeRep
wrapSomeTypeRep t = mkSomeTypeRep t

-- | Extract a TypeRep from a SomeTypeRep
unwrapSomeTypeRep :: forall r. SomeTypeRep -> (forall a. TypeRep a -> r) -> r
unwrapSomeTypeRep t f = runSomeTypeRep f t

-- | Compare unindexed typereps
eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Boolean
eqSomeTypeRep s1 s2 = unwrapSomeTypeRep s1 \t1 -> unwrapSomeTypeRep s2 \t2 -> eqTypeRep t1 t2

-- | Every Tag can be converted to the corresponding TypeRep
typeRepFromTag :: forall a. Tag a -> TypeRep a
typeRepFromTag = unsafeCoerce

-- MACHINERY + INSTANCES

typeRepFromTag1 :: forall a b. Tagged a => Typeable b => TypeRep (a b)
typeRepFromTag1 = typeRepFromTag (tag1Impl tag typeRep)

foreign import showTypeRep :: forall a. TypeRep a -> String

-- | An Opaque Tag type
foreign import data Tag :: forall k. k -> Type

-- | This is the only way to create Tags
-- | It's a function so that it can never be inlined by the compiler.
-- | This function returns unique values that are never equal.
foreign import makeTag :: forall t. Unit -> Tag t

-- | This class should only be used to specify instances for your own datatypes to automatically get Typeable instances
-- | It's never necessary to use Tagged as a constraint in order to use Typeable
class Tagged :: forall k. k -> Constraint
class Tagged a where
  tag :: Tag a

foreign import tag1Impl :: forall t a. Tag t -> TypeRep a -> Tag (t a)

instance typeableRecord :: (RL.RowToList rs ls, TypeableRecordFields ls) => Typeable (Record rs) where
  typeRep = typeRowToTypeRep (typeableRecordFields (Proxy :: _ ls))
else instance typeableTag1 :: (Tagged t, Typeable a) => Typeable (t a) where
  typeRep = typeRepFromTag1
else instance typeableTag0 :: Tagged t => Typeable t where
  typeRep = typeRepFromTag tag

instance tag1 :: (Tagged t, Typeable a) => Tagged (t a) where
  tag = tag1Impl tag typeRep

-- COMMON INSTANCES

data TypeRow :: forall k. k -> Type
data TypeRow r

typeRowToTypeRep :: forall r rl. RL.RowToList r rl => TypeRow rl -> TypeRep (Record r)
typeRowToTypeRep = typeRowToTypeRepImpl

foreign import typeRowToTypeRepImpl :: forall r rl. TypeRow rl -> TypeRep (Record r)

foreign import typeRowNil :: TypeRow RL.Nil
foreign import typeRowCons :: forall s t rs. Proxy s -> String -> TypeRep t -> TypeRow rs -> TypeRow (RL.Cons s t rs)

-- | A class for records where all fields have `Typeable` instances, used to
-- | implement the `Typeable` instance for records.
class TypeableRecordFields :: forall k. k -> Constraint
class TypeableRecordFields rowlist where
  typeableRecordFields :: Proxy rowlist -> TypeRow rowlist

instance typeableRecordFieldsNil :: TypeableRecordFields RL.Nil where
  typeableRecordFields _ = typeRowNil

instance typeableRecordFieldsCons ::
  ( IsSymbol key
  , TypeableRecordFields rowlistTail
  , Typeable focus
  ) =>
  TypeableRecordFields (RL.Cons key focus rowlistTail) where
  typeableRecordFields _ = typeRowCons key (reflectSymbol key) (typeRep :: _ focus) tail
    where
    key = Proxy :: _ key
    tail = typeableRecordFields (Proxy :: _ rowlistTail)

instance taggedInt :: Tagged Int where
  tag = makeTag unit

instance tagBoolean :: Tagged Boolean where
  tag = makeTag unit

instance tagNumber :: Tagged Number where
  tag = makeTag unit

instance tagChar :: Tagged Char where
  tag = makeTag unit

instance tagString :: Tagged String where
  tag = makeTag unit

instance tagUnit :: Tagged Unit where
  tag = makeTag unit

instance taggedArray :: Tagged Array where
  tag = makeTag unit

instance taggedMaybe :: Tagged Maybe where
  tag = makeTag unit

instance tag2Func :: Tagged (->) where
  tag = makeTag unit

instance tag2Either :: Tagged Either where
  tag = makeTag unit

instance tagOrdering :: Tagged Ordering where
  tag = makeTag unit
