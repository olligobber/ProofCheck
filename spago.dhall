{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "argonaut-core"
  , "console"
  , "control"
  , "dom-filereader"
  , "dom-indexed"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "halogen"
  , "media-types"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "promises"
  , "psci-support"
  , "strings"
  , "stringutils"
  , "web-file"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
