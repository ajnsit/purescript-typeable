module Data.Dynamic where

import Data.Exists (Exists, mkExists, runExists)
import Data.Function ((#))
import Data.Functor (map)
import Data.Leibniz (runLeibniz)
import Data.Maybe (Maybe)
import Data.Typeable (class Typeable, TypeRep, eqT, typeRep)

-- | A `Dynamic` holds a value `a` in a context `t`
-- | and forgets the type of `a`
data Dynamic' :: forall k. (k -> Type) -> k -> Type
data Dynamic' t a = Dynamic' (TypeRep a) (t a)

data Dynamic t = Dynamic (Exists (Dynamic' t))

-- | Wrap a value into a dynamic
dynamic :: forall t a. Typeable a => t a -> Dynamic t
dynamic a = Dynamic (mkExists (Dynamic' typeRep a))

-- | Extract a value from a dynamic
unwrapDynamic :: forall t a. TypeRep a -> Dynamic t -> Maybe (t a)
unwrapDynamic ta (Dynamic e) = e # runExists \(Dynamic' ti v) -> map (\w -> runLeibniz w v) (eqT ti ta)
