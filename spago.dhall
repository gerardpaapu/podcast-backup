{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "avar"
  , "console"
  , "effect"
  , "node-fs"
  , "node-http"
  , "node-streams"
  , "psci-support"
  , "simple-json"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
