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
, Proxy0, Proxy1, Proxy2, Proxy3, Proxy4, Proxy5, Proxy6, Proxy7, Proxy8, Proxy9, Proxy10, Proxy11
, proxy0, proxy1, proxy2, proxy3, proxy4, proxy5, proxy6, proxy7, proxy8, proxy9, proxy10, proxy11
, class Tag0, class Tag1, class Tag2, class Tag3, class Tag4, class Tag5, class Tag6, class Tag7, class Tag8, class Tag9, class Tag10, class Tag11
, tag0, tag1, tag2, tag3, tag4, tag5, tag6, tag7, tag8, tag9, tag10, tag11
) where

import Control.Category (identity)
import Data.Boolean (otherwise)
import Data.Either (Either)
import Data.Exists (Exists, mkExists, runExists)
import Data.Function ((#))
import Data.Functor (map)
import Data.Identity (Identity(..))
import Data.Leibniz (type (~), Leibniz, runLeibniz)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ordering (Ordering)
import Data.Show (class Show)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Unit (Unit)
import Prim.RowList as RL
import Type.Data.RowList (RLProxy(..))
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

gcast1 :: forall s t a c. Typeable a => Tag1 c => Tag1 s => Tag1 t => c (s a) -> Maybe (c (t a))
gcast1 a = m # map \f -> runLeibniz f a
  where
  m = eqT (typeRep :: _ (s a)) (typeRep :: _ (t a))

gcast2 :: forall s t a b c. Typeable a => Typeable b => Tag1 c => Tag2 s => Tag2 t => c (s a b) -> Maybe (c (t a b))
gcast2 a = m # map \f -> runLeibniz f a
  where
  m = eqT (typeRep :: _ (s a b)) (typeRep :: _ (t a b))

-- | Compare two `TypeRep`, potentially of differing types, for equality
foreign import eqTypeRep :: forall a b. TypeRep a -> TypeRep b -> Boolean

-- | Get the `TypeRep` for a value
typeRepFromVal :: forall a. Typeable a => a -> TypeRep a
typeRepFromVal _ = typeRep

-- | Unindexed typereps
data SomeTypeRep = SomeTypeRep (Exists TypeRep)

wrapSomeTypeRep :: forall a. TypeRep a -> SomeTypeRep
wrapSomeTypeRep t = SomeTypeRep (mkExists t)

-- | Unwrap a TypeRep
unwrapSomeTypeRep :: forall r. SomeTypeRep -> (forall a. TypeRep a -> r) -> r
unwrapSomeTypeRep (SomeTypeRep e) f = e # runExists f

-- MACHINERY + INSTANCES

foreign import typeRepDefault0 :: forall a. Tag0 a => TypeRep a
foreign import typeRepFromTag1 :: forall a b. Tag1 a => Typeable b => TypeRep (a b)
foreign import showTypeRep :: forall a. TypeRep a -> String

-- Tagging types
-- These would be a lot simpler after PolyKinds
data Proxy0  (t :: Type)
data Proxy1  (t :: Type -> Type)
data Proxy2  (t :: Type -> Type -> Type)
data Proxy3  (t :: Type -> Type -> Type -> Type)
data Proxy4  (t :: Type -> Type -> Type -> Type -> Type)
data Proxy5  (t :: Type -> Type -> Type -> Type -> Type -> Type)
data Proxy6  (t :: Type -> Type -> Type -> Type -> Type -> Type -> Type)
data Proxy7  (t :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type)
data Proxy8  (t :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type)
data Proxy9  (t :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type)
data Proxy10 (t :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type)
data Proxy11 (t :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type)

foreign import proxy0  :: forall t. Proxy0 t
foreign import proxy1  :: forall t. Proxy1 t
foreign import proxy2  :: forall t. Proxy2 t
foreign import proxy3  :: forall t. Proxy3 t
foreign import proxy4  :: forall t. Proxy4 t
foreign import proxy5  :: forall t. Proxy5 t
foreign import proxy6  :: forall t. Proxy6 t
foreign import proxy7  :: forall t. Proxy7 t
foreign import proxy8  :: forall t. Proxy8 t
foreign import proxy9  :: forall t. Proxy9 t
foreign import proxy10 :: forall t. Proxy10 t
foreign import proxy11 :: forall t. Proxy11 t

class Tag0  a where tag0 :: Proxy0 a
class Tag1  (a :: Type -> Type) where tag1 :: Proxy1 a
class Tag2  (a :: Type -> Type -> Type) where tag2 :: Proxy2 a
class Tag3  (a :: Type -> Type -> Type -> Type) where tag3 :: Proxy3 a
class Tag4  (a :: Type -> Type -> Type -> Type -> Type) where tag4 :: Proxy4 a
class Tag5  (a :: Type -> Type -> Type -> Type -> Type -> Type) where tag5 :: Proxy5 a
class Tag6  (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type) where tag6 :: Proxy6 a
class Tag7  (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type) where tag7 :: Proxy7 a
class Tag8  (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type) where tag8 :: Proxy8 a
class Tag9  (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type) where tag9 :: Proxy9 a
class Tag10 (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type) where tag10 :: Proxy10 a
class Tag11 (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type) where tag11 :: Proxy11 a

-- foreign import proxy0FromTag1 :: forall t a. Tag1 t => Typeable a => Proxy0 (t a)
foreign import proxy1FromTag2 :: forall t a. Tag2 t => Typeable a => Proxy1 (t a)
foreign import proxy2FromTag3 :: forall t a. Tag3 t => Typeable a => Proxy2 (t a)
foreign import proxy3FromTag4 :: forall t a. Tag4 t => Typeable a => Proxy3 (t a)
foreign import proxy4FromTag5 :: forall t a. Tag5 t => Typeable a => Proxy4 (t a)
foreign import proxy5FromTag6 :: forall t a. Tag6 t => Typeable a => Proxy5 (t a)
foreign import proxy6FromTag7 :: forall t a. Tag7 t => Typeable a => Proxy6 (t a)
foreign import proxy7FromTag8 :: forall t a. Tag8 t => Typeable a => Proxy7 (t a)
foreign import proxy8FromTag9 :: forall t a. Tag9 t => Typeable a => Proxy8 (t a)
foreign import proxy9FromTag10 :: forall t a. Tag10 t => Typeable a => Proxy9 (t a)
foreign import proxy10FromTag11 :: forall t a. Tag11 t => Typeable a => Proxy10 (t a)

instance typeableRecord :: (RL.RowToList rs ls, TypeableRecordFields ls) => Typeable (Record rs) where
  typeRep = typeRowToTypeRep (typeableRecordFields (RLProxy :: _ ls))
else
instance tag0FromTag1 :: (Tag1 t, Typeable a) => Typeable (t a) where
  typeRep = typeRepFromTag1
else
instance typeableTag0 :: Tag0 t => Typeable t where
  typeRep = typeRepDefault0

instance tag1FromTag2 :: (Tag2 t, Typeable a) => Tag1 (t a) where
  tag1 = proxy1FromTag2

instance tag2FromTag3 :: (Tag3 t, Typeable a) => Tag2 (t a) where
  tag2 = proxy2FromTag3

instance tag3FromTag4 :: (Tag4 t, Typeable a) => Tag3 (t a) where
  tag3 = proxy3FromTag4

instance tag4FromTag5 :: (Tag5 t, Typeable a) => Tag4 (t a) where
  tag4 = proxy4FromTag5

instance tag5FromTag6 :: (Tag6 t, Typeable a) => Tag5 (t a) where
  tag5 = proxy5FromTag6

instance tag6FromTag7 :: (Tag7 t, Typeable a) => Tag6 (t a) where
  tag6 = proxy6FromTag7

instance tag7FromTag8 :: (Tag8 t, Typeable a) => Tag7 (t a) where
  tag7 = proxy7FromTag8

instance tag8FromTag9 :: (Tag9 t, Typeable a) => Tag8 (t a) where
  tag8 = proxy8FromTag9

instance tag9FromTag10 :: (Tag10 t, Typeable a) => Tag9 (t a) where
  tag9 = proxy9FromTag10

instance tag10FromTag11 :: (Tag11 t, Typeable a) => Tag10 (t a) where
  tag10 = proxy10FromTag11

-- COMMON INSTANCES

-- TODO: Don't know how to use a Row instead of a RowList here
-- (r :: RL.RLProxy)
data TypeRow :: forall k. k -> Type
data TypeRow r
foreign import typeRowToTypeRep :: forall r rl. RL.RowToList r rl => TypeRow rl -> TypeRep (Record r)
foreign import typeRowNil :: TypeRow RL.Nil
foreign import typeRowCons :: forall s t rs. SProxy s -> String -> TypeRep t -> TypeRow rs -> TypeRow (RL.Cons s t rs)

-- | A class for records where all fields have `Typeable` instances, used to
-- | implement the `Typeable` instance for records.
class TypeableRecordFields rowlist where
  typeableRecordFields :: RLProxy rowlist -> TypeRow rowlist

instance typeableRecordFieldsNil :: TypeableRecordFields RL.Nil where
  typeableRecordFields _ = typeRowNil

instance typeableRecordFieldsCons
    :: ( IsSymbol key
       , TypeableRecordFields rowlistTail
       , Typeable focus
       )
    => TypeableRecordFields (RL.Cons key focus rowlistTail) where
  typeableRecordFields _
    = typeRowCons key (reflectSymbol key) (typeRep :: _ focus) tail
    where
      key = SProxy :: _ key
      tail = typeableRecordFields (RLProxy :: _ rowlistTail)

instance taggedInt :: Tag0 Int where
  tag0 = proxy0

instance tag0Boolean :: Tag0 Boolean where
  tag0 = proxy0

instance tag0Number :: Tag0 Number where
  tag0 = proxy0

instance tag0Char :: Tag0 Char where
  tag0 = proxy0

instance tag0String :: Tag0 String where
  tag0 = proxy0

instance tag0Unit :: Tag0 Unit where
  tag0 = proxy0

instance taggedArray :: Tag1 Array where
  tag1 = proxy1

instance taggedMaybe :: Tag1 Maybe where
  tag1 = proxy1

instance tag2Func :: Tag2 (->) where
  tag2 = proxy2

instance tag2Either :: Tag2 Either where
  tag2 = proxy2

instance tag0Ordering :: Tag0 Ordering where
  tag0 = proxy0
