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
-- , SomeTypeRep(..)
-- , wrapSomeTypeRep
-- , unwrapSomeTypeRep
-- , eqSomeTypeRep
, ProxyT, proxyT
, TagTDict
, class TagT, tagTDict
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
import Data.Unit (Unit)
import Prim.RowList as RL
import Unsafe.Coerce (unsafeCoerce)
import Type.Proxy

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

gcast1 :: forall s t a c. Typeable a => Typeable (s a) => Typeable (t a) => TagT c => TagT s => TagT t => c (s a) -> Maybe (c (t a))
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

-- -- | Unindexed typereps
-- data SomeTypeRep = SomeTypeRep (forall k r. (forall (a :: k). TypeRep a -> r) -> r)

-- -- | Wrap a TypeRep into a SomeTypeRep
-- wrapSomeTypeRep :: forall a. TypeRep a -> SomeTypeRep
-- wrapSomeTypeRep t = SomeTypeRep (\f -> f t)

-- -- | Extract a TypeRep from a SomeTypeRep
-- unwrapSomeTypeRep :: forall r. SomeTypeRep -> (forall a. TypeRep a -> r) -> r
-- unwrapSomeTypeRep (SomeTypeRep e) f = e f

-- -- | Compare unindexed typereps
-- eqSomeTypeRep :: SomeTypeRep -> SomeTypeRep -> Boolean
-- eqSomeTypeRep s1 s2 = unwrapSomeTypeRep s1 \t1 -> unwrapSomeTypeRep s2 \t2 -> eqTypeRep t1 t2

-- MACHINERY + INSTANCES

typeRepDefault0 :: forall a. TagT a => TypeRep a
typeRepDefault0 = foreign_typeRepDefault0 tagTDict

foreign import foreign_typeRepDefault0 :: forall a. TagTDict a -> TypeRep a

typeRepFromTag1 :: forall a b. TagT a => Typeable b => TypeRep (a b)
typeRepFromTag1 = foreign_typeRepFromTag1 tagTDict typeRep

foreign import foreign_typeRepFromTag1 :: forall a b. TagTDict a -> TypeRep b -> TypeRep (a b)

foreign import showTypeRep :: forall a. TypeRep a -> String

-- Tagging types, this is basically the same as `Type.Proxy`
-- but we don't want to export any constructors
data ProxyT :: forall k. k -> Type
data ProxyT t

foreign import proxyT :: forall t. ProxyT t

type TagTDict :: forall k. k -> Type
type TagTDict a = { tagT :: ProxyT a }

class TagT :: forall k. k -> Constraint
class TagT a where
  tagTDict :: TagTDict a

proxyTFromTagT :: forall t a. TagT t => Typeable a => ProxyT (t a)
proxyTFromTagT = foreign_proxyTFromTagT tagTDict typeRep

foreign import foreign_proxyTFromTagT :: forall t a. TagTDict t -> TypeRep a -> ProxyT (t a)

instance typeableRecord :: (RL.RowToList rs ls, TypeableRecordFields ls) => Typeable (Record rs) where
  typeRep = typeRowToTypeRep (typeableRecordFields (Proxy :: _ ls))
else
instance typeableTag1 :: (TagT t, Typeable a) => Typeable (t a) where
  typeRep = typeRepFromTag1
else
instance typeableTag0 :: TagT t => Typeable t where
  typeRep = typeRepDefault0

instance tagTFromTagT :: (TagT t, Typeable a) => TagT (t a) where
  tagTDict = { tagT: proxyTFromTagT }

-- COMMON INSTANCES

-- TODO: Don't know how to use a Row instead of a RowList here
-- (r :: RL.RLProxy)
data TypeRow :: forall k. k -> Type
data TypeRow r

typeRowToTypeRep :: forall r rl. RL.RowToList r rl => TypeRow rl -> TypeRep (Record r)
typeRowToTypeRep = foreign_typeRowToTypeRep

foreign import foreign_typeRowToTypeRep :: forall r rl. TypeRow rl -> TypeRep (Record r)

foreign import typeRowNil :: TypeRow RL.Nil
foreign import typeRowCons :: forall s t rs. Proxy s -> String -> TypeRep t -> TypeRow rs -> TypeRow (RL.Cons s t rs)

-- | A class for records where all fields have `Typeable` instances, used to
-- | implement the `Typeable` instance for records.
class TypeableRecordFields rowlist where
  typeableRecordFields :: Proxy rowlist -> TypeRow rowlist

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
      key = Proxy :: _ key
      tail = typeableRecordFields (Proxy :: _ rowlistTail)

instance TagT Int where
  tagTDict = { tagT: proxyT }

instance TagT Boolean where
  tagTDict = { tagT: proxyT }

instance TagT Number where
  tagTDict = { tagT: proxyT }

instance TagT Char where
  tagTDict = { tagT: proxyT }

instance TagT String where
  tagTDict = { tagT: proxyT }

instance TagT Unit where
  tagTDict = { tagT: proxyT }

instance TagT Array where
  tagTDict = { tagT: proxyT }

instance TagT Maybe where
  tagTDict = { tagT: proxyT }

instance TagT (->) where
  tagTDict = { tagT: proxyT }

instance TagT Either where
  tagTDict = { tagT: proxyT }

instance TagT Ordering where
  tagTDict = { tagT: proxyT }
