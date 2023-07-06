# Purescript-Typeable
Reified types for Purescript

This is an implementation of indexed typereps for Purescript, similar to the [corresponding implementation in Haskell](https://hackage.haskell.org/package/base/docs/Type-Reflection.html#t:TypeRep).

[Slides for a talk about Purescript-Typeable](https://speakerdeck.com/ajnsit/purescript-typeable), presented at the Purescript semi-monthly meetup on 18 January 2021, are available.

## Data.Typeable

`TypeReps` are values that represent types (i.e. they reify types). When they are *indexed* they have the type itself as a parameter.

```purescript
data TypeRep a -- A *value* that represents the type 'a'
```

All typeable things have typereps -

```purescript
class Typeable a where
  typeRep :: TypeRep a
```

Instances are provided for common data types.

We can recover the unindexed representation by making it existential -

```purescript
data SomeTypeRep
```

We can also test any two typereps for equality -

```purescript
eqTypeRep :: forall a b. TypeRep a -> TypeRep b -> Boolean
```

We can compare two typeReps and extract a witness for type equality.

```purescript
eqT :: forall a b. TypeRep a -> TypeRep b -> Maybe (a ~ b)
```

## Deriving `Typeable` for custom data types

It's extremely easy. You just need to create a mechanical `Taggable` class instance for your datatype. The instance will always use the provided `makeTag` function. There is no other possible way to create an instance.

For example -

```purescript
data Person = Person {name::String, age::Int}

instance Taggable Person where tag = makeTag unit
```

This is valid even for data types that take parameters. For example -

```purescript
data Optional a = Some a | None

instance Taggable Optional where tag = makeTag unit
```

**Don't worry about getting it wrong since the type system will prevent you from writing an invalid instance.**

> #### CAVEAT
> *Do not add any extra constraints to the instances*. For example don't do `Foo => Taggable Person`. This currently cannot be caught by the type checker, but will break typerep comparisons for your data type.

And that's it! You are done! Now your datatype will have a `Typeable` instance.

Note that you will have `Typeable` instances even for unsaturated types. For example, with the `tagTOptional` instance above, you have instances for `TypeRep (Optional a)` as well as for `TypeRep Optional`.

## Data.Dynamic

We can have dynamic values which holds a value `a` in a context `t` and forgets the type of `a`

```purescript
data Dynamic t
```

We can wrap a value into a dynamic

```purescript
dynamic :: forall a t. Typeable a => t a -> Dynamic t
```

We can recover the value from a dynamic if supply the type we expect to find in the Dynamic

```purescript
unwrapDynamic :: forall a. TypeRep a -> Dynamic t -> Maybe a
```

## Data.Data

This is an implementation of the Data class Purescript, similar to the [corresponding implementation of Data in Haskell](https://hackage.haskell.org/package/base/docs/Data-Data.html). Check the documentation there for more information on the API. A brief overview is provided below.

```purescript
class Typeable a <= Data a where
  dataDict :: DataDict a
```

Where `DataDict` is a manually reified dictionary because PureScript has trouble with constraints inside records.

Common instances are provided for `Data`. You can define `Data` instances for your own datatypes though it is slightly involved due to the dictionary reification.

When you cut through the dictionary reification noise, the definition of `Data` looks like the following -

```purescript
class Typeable a => Data a where
  gmapT :: (forall b. Data b => b -> b) -> a -> a
```

Basically `Data` encodes a traversal through the structure of the data.

Let's say you have the following data structure -

```purescript
data Foo = Foo Bar Baz
```

You would define a `Data` instance for `Foo` like the following -

```purescript
instance Data Foo where
  gmapT k (Foo a b) = Foo (k a) (k b)
```

i.e. You apply the supplied function to the immediate children of the top level structure.

Now because of the dictionary reification in PureScript, you can't directly write the instance that way. You need to do the following instead -

```purescript
instance Data Foo where
  dataDict = DataDict \k z (Foo a b) -> z Foo `k` a `k` b
```

i.e. You are doing the same traversal, but start with the `z`, and intersperse all the immediate children of the data structure with `k`.

If your data structure has multiple branches, for example -

```purescript
data Foo = Bar Baz | Buzz Int
```

Simply handle the branches separately, for example -

```purescript
instance Data Foo where
  dataDict = DataDict \k z foo -> case foo of
    Bar b -> z Bar `k` b
    Buzz i -> z Buzz `k` i
```

### Using Data.Data

Once you have a `Data` instance for your datatype, you can use `gmapT`, and functions that depend on `gmapT`, such as `everywhere`.

For example, to apply a function `f :: forall a. Typeable a => a -> a` to all leaves of your data structure, you can do `everywhere f`. Inside `f`, you can use the `Typeable` instance to decide when to change the data structure. For example, if you want the function to increment all integers, but leave all other data intact, use -

```purescript
f :: forall a. Typeable a => a -> a
f a =
  -- Check if `a` is an int
  case (typeRep :: _ a) `eqT` (typeRep :: _ Int) of
    -- Return unmodified if `a` is not an int
    Nothing -> a
    Just witness -> do
      -- If `a` is an int, we get access to bidirectional conversion functions
      let aToI = coerce witness
      let iToA = coerce (symm witness)
      -- Increment and return
      iToA (aToI a + 1)
```

