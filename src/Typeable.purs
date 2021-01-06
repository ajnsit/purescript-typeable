module Typeable where

import Data.Unit (Unit)
import Effect (Effect)

-- Indexed Typereps
data TypeRep a

-- Typeable class
class Typeable a where
  typeRep :: TypeRep a

-- Implementation detail
-- Tagging types
data Proxy1 a = Proxy1
data Proxy2 (t :: Type -> Type) = Proxy2
data Proxy3 (t :: Type -> Type -> Type) = Proxy3
class Tag1 a where t1 :: Proxy1 a
class Tag2 (a :: Type -> Type) where t2 :: Proxy2 a
class Tag3 (a :: Type -> Type -> Type) where t3 :: Proxy3 a

-- Constructors for the opaque data type
foreign import typerepImpl1 :: forall a. Tag1 a => TypeRep a
foreign import typerepImpl2 :: forall a b. Typeable b => Tag2 a => TypeRep (a b)
foreign import typerepImpl3 :: forall a b c. Typeable c => Typeable b => Tag1 c => TypeRep (a b c)

-- Type equality
foreign import eqTypeRep :: forall a b. TypeRep a -> TypeRep b -> Boolean

-- Common instances
instance tag1Int :: Tag1 Int where t1 = Proxy1
instance typeableInt :: Typeable Int where typeRep = typerepImpl1
instance tag2Array :: Tag2 Array where t2 = Proxy2
instance typeableArray :: Typeable a => Typeable (Array a) where typeRep = typerepImpl2

-- DEBUG: Console.log anything
foreign import clog :: forall a. a -> Effect Unit
