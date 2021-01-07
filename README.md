# Purescript-Typeable
Reified types for Purescript

This is an implementation of indexed typereps for Purescript, similar to the [corresponding implementation in Haskell](https://hackage.haskell.org/package/base-4.10.0.0/docs/Type-Reflection.html#t:TypeRep).

## API

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

## Mini tutorial on using Typeable and TypeReps

As an example, with typereps we can have dynamic values which carry around the type information as a value -

```purescript
data Dynamic' a = Dynamic' (TypeRep a) a
data Dynamic = Dynamic (Exists Dynamic')

-- Wrap a value into a dynamic
dynamic :: forall a. 
dynamic a = Dynamic (mkExists (Dynamic' typeRep a)) -- The compiler picks the appropriate typerep automatically
```

To get a value out of a dynamic, we must have a way to convert a `TypeRep` into typelevel information. We do this with *witnesses*.
However witnesses require GADTs, and since Purescript does not have GADTs, we hack around it with Leibniz equality -

```purescript
-- | Two types are equal if they are _equal in all contexts_.
newtype Leibniz a b = Leibniz (forall f. f a -> f b)
infix 4 type Leibniz as ~
```

Now we can compare two typereps and recover type information -

```purescript
eqT :: forall a b. TypeRep a -> TypeRep b -> Maybe (a ~ b)
eqT ta tb
  | eqTypeRep ta tb = Just (unsafeCoerce identity)
  | otherwise = Nothing
```

And use this information to unwrap a dynamic value. We need to pass the type we expect to find in the dynamic -

```purescript
unwrapDynamic :: forall a. TypeRep a -> Dynamic -> Maybe a
unwrapDynamic ta (Dynamic e) = runExists e \(Dynamic ta tb) -> case eqT ta tb of
  Nothing -> Nothing
  Just witness -> runLeibniz witness tb
```

## Deriving `Typeable` for custom data types

### Create a `Tag` instance for your data type

First create a mechanical `Tag` class instance for your datatype. There are different `Tag` classes for types of different arity.

For example, to derive an instance for a plain data type, use `Tag1` and `Proxy1` -

```purescript
data Person = Person {name::String, age::Int}

instance tag1Person :: Tag1 Person where t1 = Proxy1
```

For a data type which takes one type parameter, use `Tag2` and `Proxy2`, and so on -

```purescript
data Optional a = Some a | None

instance tag2Optional :: Tag2 Optional where t2 = Proxy2
```

Don't worry about getting it wrong since the type system will prevent you from writing an invalid instance.

> #### CAVEATS
> There are two cases where the compiler would not prevent you from writing an invalid instance. Without compiler support it's very hard to prevent these mistakes at compile time.
> However the good thing is that these mistakes are hard to make unless you are actively looking to subvert the type system, and accidental mistakes will be caught at compile time.
> 
> 1. *Do not include type parameters in your instance*.
> 
>     For example do not create an instance for `Tag1 (Optional a)`.
>     
>     That would give the same typerep to all `Optional`s, for example `Optional Int` and `Optional String` would have the same typerep.
> 
>     The compiler would prevent you from including type params in your instance *accidentally*, for example you would get an error on defining an instance for `Tag2 (Optional a)`.
> 
> 2. *Do not add any extra constraints to the instances*. For example don't do `Foo => Tag1 Person`.

### Then create a `Typeable` instance for your datatype

Write a mechanical `Typeable` instance for your datatype -

1. Use one of the baked in `typerepImpl` functions as the implementation for your instance. For example, use `typerepImpl1` for plain types which take no type arguments, use `typerepImpl2` for types that take one argument, and so on.
2. Add constrinats to make sure that all type parameters are `Typeable` too.
3. Add a constraint for appropriate tag instance as a constraint.

Again, don't worry about getting it wrong since the type system will prevent you from writing an invalid instance.

```purescript
instance typeablePerson :: Typeable Person where typeRep = typerepImpl1
```

```purescript
instance typeableOptional :: Typeable a => Typeable (Optional a) where typeRep = typerepImpl2
```

And that's it! You are done!
