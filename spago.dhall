{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "ansi"
  , "argonaut-core"
  , "argonaut-generic"
  , "console"
  , "effect"
  , "halogen"
  , "node-fs"
  , "node-readline"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "queue"
  , "random"
  , "strings"
  , "test-unit"
  , "unicode"
  , "validation"
  , "yargs"
  , "zeta"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
