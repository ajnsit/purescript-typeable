{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "typeable"
, dependencies =
  [ "effect"
  , "psci-support"
  , "either"
  , "exists"
  , "leibniz"
  , "prelude"
  , "tuples"
  , "arrays"
  , "const"
  , "control"
  , "foldable-traversable"
  , "identity"
  , "maybe"
  , "newtype"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
