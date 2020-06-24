{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut-core"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "halogen"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "strings"
  , "stringutils"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
