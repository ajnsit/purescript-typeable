module Data.Data where

import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Applicative (class Applicative, pure)
import Control.Bind (bind, (>>=))
import Control.Category (identity, (<<<))
import Control.Monad (class Monad)
import Control.MonadPlus (class MonadPlus)
import Data.Array as A
import Data.CommutativeRing ((+))
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Foldable (foldl)
import Data.Function (const, ($))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Ordering (Ordering)
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, cast)
import Data.Unit (Unit)
import Unsafe.Coerce (unsafeCoerce)

mkT :: forall a b. Typeable a => Typeable b => (b -> b) -> a -> a
mkT f = fromMaybe identity (cast f)

mkQ :: forall a b r. Typeable a => Typeable b => r -> (b -> r) -> a -> r
mkQ r q a = maybe r q (cast a)

mkM :: forall a b m. Typeable a => Typeable b => Typeable (m a) => Typeable (m b) => Applicative m => (b -> m b) -> a -> m a
mkM f = fromMaybe pure (cast f)

-- Purescript can't have cycles in typeclasses
-- So we manually reify the dictionary into the DataDict datatype
-- Not using wrapped records here because Purescript can't handle constraints inside records
newtype DataDict a = DataDict
  ( forall c
     . (forall d b. Data d => c (d -> b) -> d -> c b)
    -- ^ defines how nonempty constructor applications are
    -- folded.  It takes the folded tail of the constructor
    -- application and its head, i.e., an immediate subterm,
    -- and combines them in some way.
    -> (forall g. g -> c g)
    -- ^ defines how the empty constructor application is
    -- folded, like the neutral \/ start element for list
    -- folding.
    -> a
    -- ^ structure to be folded.
    -> c a
  -- ^ result, with a type defined in terms of @a@, but
  -- variability is achieved by means of type constructor
  -- @c@ for the construction of the actual result type.
  )

gfoldl
  :: forall a c
   . Data a
  => (forall d b. Data d => c (d -> b) -> d -> c b)
  -- ^ defines how nonempty constructor applications are
  -- folded.  It takes the folded tail of the constructor
  -- application and its head, i.e., an immediate subterm,
  -- and combines them in some way.
  -> (forall g. g -> c g)
  -- ^ defines how the empty constructor application is
  -- folded, like the neutral \/ start element for list
  -- folding.
  -> a
  -- ^ structure to be folded.
  -> c a
-- ^ result, with a type defined in terms of @a@, but
-- variability is achieved by means of type constructor
-- @c@ for the construction of the actual result type.
gfoldl = let DataDict f = dataDict in f

class Typeable a <= Data a where
  dataDict :: DataDict a

-- | A generic transformation that maps over the immediate subterms
--
-- The default definition instantiates the type constructor @c@ in the
-- type of 'gfoldl' to an identity datatype constructor, using the
-- isomorphism pair as injection and projection.
gmapT :: forall a. Data a => (forall b. Data b => b -> b) -> a -> a

-- Use the Identity datatype constructor
-- to instantiate the type constructor c in the type of gfoldl,
-- and perform injections Identity and projections runIdentity accordingly.
--
gmapT f x0 = unwrap (gfoldl k Identity x0)
  where
  k :: forall d b. Data d => Identity (d -> b) -> d -> Identity b
  k (Identity c) x = Identity (c (f x))

-- | A generic query with a left-associative binary operator
gmapQl :: forall a r r'. Data a => (r -> r' -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
gmapQl o r f = unwrap <<< gfoldl k z
  where
  k :: forall d b. Data d => Const r (d -> b) -> d -> Const r b
  k c x = Const $ (unwrap c) `o` f x

  z :: forall g. g -> Const r g
  z _ = Const r

-- | A generic query with a right-associative binary operator
gmapQr :: forall a r r'. Data a => (r' -> r -> r) -> r -> (forall d. Data d => d -> r') -> a -> r
gmapQr o r0 f x0 = unQr (gfoldl k (const (Qr identity)) x0) r0
  where
  k :: forall d b. Data d => Qr r (d -> b) -> d -> Qr r b
  k (Qr c) x = Qr (\r -> c (f x `o` r))

-- | A generic query that processes the immediate subterms and returns a list
-- of results.  The list is given in the same order as originally specified
-- in the declaration of the data constructors.
gmapQ :: forall a u. Data a => (forall d. Data d => d -> u) -> a -> Array u
gmapQ f = gmapQr (A.cons) [] f

-- | A generic query that processes one child by index (zero-based)
gmapQi :: forall u a. Data a => Int -> (forall d. Data d => d -> u) -> a -> u
gmapQi i f x = case gfoldl k z x of
  Qi _ q -> case q of
    Nothing -> unsafeCoerce "UNEXPECTED NOTHING"
    Just q' -> q'
  where
  k :: forall d b. Data d => Qi u (d -> b) -> d -> Qi u b
  k (Qi i' q) a = Qi (i' + 1) (if i == i' then Just (f a) else q)

  z :: forall g q. g -> Qi q g
  z _ = Qi 0 Nothing

-- | A generic monadic transformation that maps over the immediate subterms
--
-- The default definition instantiates the type constructor @c@ in
-- the type of 'gfoldl' to the monad datatype constructor, defining
-- injection and projection using 'return' and '>>='.
gmapM :: forall m a. Data a => Monad m => (forall d. Data d => d -> m d) -> a -> m a

-- Use immediately the monad datatype constructor
-- to instantiate the type constructor c in the type of gfoldl,
-- so injection and projection is done by return and >>=.
--
gmapM f = gfoldl k pure
  where
  k :: forall b d. Data d => m (d -> b) -> d -> m b
  k c x = do
    c' <- c
    x' <- f x
    pure (c' x')

-- | Transformation of at least one immediate subterm does not fail
gmapMp :: forall m a. Data a => MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

{-

The type constructor that we use here simply keeps track of the fact
if we already succeeded for an immediate subterm; see Mp below. To
this end, we couple the monadic computation with a Boolean.

-}

gmapMp f x = unMp (gfoldl k z x) >>= \(Tuple x' b) ->
  if b then pure x' else empty
  where
  z :: forall g. g -> Mp m g
  z g = Mp (pure (Tuple g false))

  k :: forall d b. Data d => Mp m (d -> b) -> d -> Mp m b
  k (Mp c) y = Mp
    ( c >>= \(Tuple h b) ->
        (f y >>= \y' -> pure (Tuple (h y') true))
          <|> pure (Tuple (h y) b)
    )

-- | Transformation of one immediate subterm with success
gmapMo :: forall m a. Data a => MonadPlus m => (forall d. Data d => d -> m d) -> a -> m a

{-

We use the same pairing trick as for gmapMp,
i.e., we use an extra Boolean component to keep track of the
fact whether an immediate subterm was processed successfully.
However, we cut of mapping over subterms once a first subterm
was transformed successfully.

-}

gmapMo f x = unMp (gfoldl k z x) >>= \(Tuple x' b) ->
  if b then pure x' else empty
  where
  z :: forall g. g -> Mp m g
  z g = Mp (pure (Tuple g false))

  k :: forall d b. Data d => Mp m (d -> b) -> d -> Mp m b
  k (Mp c) y = Mp
    ( c >>= \(Tuple h b) ->
        if b then pure (Tuple (h y) b)
        else (f y >>= \y' -> pure (Tuple (h y') true))
          <|> pure (Tuple (h y) b)
    )

-- | Type constructor for adding counters to queries
data Qi :: forall k. Type -> k -> Type
data Qi q a = Qi Int (Maybe q)

-- | The type constructor used in definition of gmapQr
newtype Qr :: forall k. Type -> k -> Type
newtype Qr r a = Qr (r -> r)

unQr :: forall r a. Qr r a -> r -> r
unQr (Qr f) = f

-- | The type constructor used in definition of gmapMp
newtype Mp m x = Mp (m (Tuple x Boolean))

unMp :: forall m x. Mp m x -> m (Tuple x Boolean)
unMp (Mp f) = f

-- | Left-associative fold operation for constructor applications.
--
-- The type of 'gfoldl' is a headache, but operationally it is a simple
-- generalisation of a list fold.
--
-- The default definition for 'gfoldl' is @'const' 'id'@, which is
-- suitable for abstract datatypes with no substructures.
-- gfoldl

instance dataArray :: Data a => Data (Array a) where
  dataDict = DataDict \k z arr -> case A.uncons arr of
    Nothing -> z []
    Just x -> (z A.cons `k` x.head) `k` x.tail

instance dataMaybe :: Data a => Data (Maybe a) where
  dataDict = DataDict \k z e -> case e of
    Nothing -> z Nothing
    Just a -> z Just `k` a

instance dataEither :: (Data a, Data b) => Data (Either a b) where
  dataDict = DataDict \k z e -> case e of
    Left a -> z Left `k` a
    Right b -> z Right `k` b

instance dataBoolean :: Data Boolean where
  dataDict = DataDict \_ z x -> z x

instance dataInt :: Data Int where
  dataDict = DataDict \_ z x -> z x

instance dataNumber :: Data Number where
  dataDict = DataDict \_ z x -> z x

instance dataChar :: Data Char where
  dataDict = DataDict \_ z x -> z x

instance dataString :: Data String where
  dataDict = DataDict \_ z x -> z x

instance dataUnit :: Data Unit where
  dataDict = DataDict \_ z x -> z x

instance dataOrdering :: Data Ordering where
  dataDict = DataDict \_ z x -> z x

-- Combinators

-- | Apply a transformation everywhere, bottom-up
everywhere :: forall a. Data a => (forall b. Data b => b -> b) -> a -> a
everywhere f x = f (gmapT (everywhere f) x)

-- | Apply a transformation everywhere, top-down
everywhere' :: forall a. Data a => (forall b. Data b => b -> b) -> a -> a
everywhere' f x = gmapT (everywhere' f) (f x)

-- | Summarise all nodes in top-down, left-to-right
everything :: forall a r. Data a => (r -> r -> r) -> (forall b. Data b => b -> r) -> a -> r
everything k f x = foldl k (f x) (gmapQ (everything k f) x)

-- | Apply a monadic transformation everywhere, bottom-up
everywhereM :: forall m a. Monad m => Data a => (forall b. Data b => b -> m b) -> a -> m a
everywhereM f x = gmapM (everywhereM f) x >>= f
