{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "typeable"
, dependencies =
  [ "arrays"
  , "const"
  , "control"
  , "either"
  , "exists"
  , "foldable-traversable"
  , "identity"
  , "leibniz"
  , "maybe"
  , "newtype"
  , "prelude"
  , "psci-support"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
