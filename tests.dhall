{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "typeable-tests"
, dependencies =
  [ "aff"
  , "effect"
  , "either"
  , "exceptions"
  , "prelude"
  , "spec"
  , "transformers"
  , "typeable"
  ]
, packages = ./packages.dhall with typeable = ./spago.dhall as Location
, sources = [ "test/**/*.purs" ]
}
