{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "typeable"
, dependencies = [ "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
