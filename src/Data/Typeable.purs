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
, eqSomeTypeRep
, ProxyT, proxyT
, class TagT, tagT
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

gcast1 :: forall s t a c. Typeable a => TagT c => TagT s => TagT t => c (s a) -> Maybe (c (t a))
gcast1 a = m # map \f -> runLeibniz f a
  where
  m = eqT (typeRep :: _ (s a)) (typeRep :: _ (t a))

gcast2 :: forall s t a b c. Typeable a => Typeable b => TagT c => TagT s => TagT t => c (s a b) -> Maybe (c (t a b))
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

-- | Wrap a TypeRep into a SomeTypeRep
wrapSomeTypeRep :: forall a. TypeRep a -> SomeTypeRep
wrapSomeTypeRep t = SomeTypeRep (mkExists t)

-- | Extract a TypeRep from a SomeTypeRep
unwrapSomeTypeRep :: forall r. SomeTypeRep -> (forall a. TypeRep a -> r) -> r
unwrapSomeTypeRep (SomeTypeRep e) f = e # runExists f

-- | Compare unindexed typereps
eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Boolean
eqSomeTypeRep s1 s2 = unwrapSomeTypeRep s1 \t1 -> unwrapSomeTypeRep s2 \t2 -> eqTypeRep t1 t2

-- MACHINERY + INSTANCES

foreign import typeRepDefault0 :: forall a. TagT a => TypeRep a
foreign import typeRepFromTag1 :: forall a b. TagT a => Typeable b => TypeRep (a b)
foreign import showTypeRep :: forall a. TypeRep a -> String

-- Tagging types, this is basically the same as `Type.Proxy`
-- but we don't want to export any constructors
data ProxyT :: forall k. k -> Type
data ProxyT t

foreign import proxyT  :: forall t. ProxyT t

class TagT :: forall k. k -> Constraint
class TagT  a where tagT :: ProxyT a

foreign import proxyTFromTagT :: forall t a. TagT t => Typeable a => ProxyT (t a)

instance typeableRecord :: (RL.RowToList rs ls, TypeableRecordFields ls) => Typeable (Record rs) where
  typeRep = typeRowToTypeRep (typeableRecordFields (RLProxy :: _ ls))
else
instance typeableTag1 :: (TagT t, Typeable a) => Typeable (t a) where
  typeRep = typeRepFromTag1
else
instance typeableTag0 :: TagT t => Typeable t where
  typeRep = typeRepDefault0

instance tagTFromTagT :: (TagT t, Typeable a) => TagT (t a) where
  tagT = proxyTFromTagT

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

instance taggedInt :: TagT Int where
  tagT = proxyT

instance tagTBoolean :: TagT Boolean where
  tagT = proxyT

instance tagTNumber :: TagT Number where
  tagT = proxyT

instance tagTChar :: TagT Char where
  tagT = proxyT

instance tagTString :: TagT String where
  tagT = proxyT

instance tagTUnit :: TagT Unit where
  tagT = proxyT

instance taggedArray :: TagT Array where
  tagT = proxyT

instance taggedMaybe :: TagT Maybe where
  tagT = proxyT

instance tag2Func :: TagT (->) where
  tagT = proxyT

instance tag2Either :: TagT Either where
  tagT = proxyT

instance tagTOrdering :: TagT Ordering where
  tagT = proxyT
