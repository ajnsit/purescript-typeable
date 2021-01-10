# Purescript-Typeable
Reified types for Purescript

This is an implementation of indexed typereps for Purescript, similar to the [corresponding implementation in Haskell](https://hackage.haskell.org/package/base-4.10.0.0/docs/Type-Reflection.html#t:TypeRep).

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
data SomeTypeRep = SomeTypeRep (Exists TypeRep)
```

We can also test typereps for equality -

```purescript
eqTypeRep :: forall a b. TypeRep a -> TypeRep b -> Boolean
```

We can compare two typeReps and extract a witness for type equality.

```purescript
eqT :: forall a b. TypeRep a -> TypeRep b -> Maybe (a ~ b)
```

## Data.Dynamic

We can have dynamic values which holds a value `a` in a context `t` and forgets the type of `a`

```purescript
data Dynamic t
```

We can wrap a value into a dynamic

```purescript
-- Wrap a value into a dynamic
dynamic :: forall a t. Typeable a => t a -> Dynamic t
```

We can recover a value out of a dynamics if supply the type we expect to find in the Dynamic

```purescript
unwrapDynamic :: forall a. TypeRep a -> Dynamic t -> Maybe a
```

## Deriving `Typeable` for custom data types

It's extremely easy. You just need to create a mechanical `Tag` class instance for your datatype. There are different `Tag` classes for types of different arity.

For example, to derive an instance for a plain data type, use `Tag1` and `proxy1` -

```purescript
data Person = Person {name::String, age::Int}

instance tag1Person :: Tag1 Person where t1 = proxy1
```

For a data type which takes one type parameter, use `Tag2` and `proxy2`, and so on -

```purescript
data Optional a = Some a | None

instance tag2Optional :: Tag2 Optional where t2 = proxy2
```

**Don't worry about getting it wrong since the type system will prevent you from writing an invalid instance.**

> #### CAVEAT
> *Do not add any extra constraints to the instances*. For example don't do `Foo => Tag1 Person`. This currently cannot be caught by the type checker, but will break typerep comparisons for your data type.

And that's it! You are done! Now your datatype will have a `Typeable` instance.
