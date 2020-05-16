{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-halogen-project"
, dependencies =
  [ "console"
  , "effect"
  , "generics-rep"
  , "halogen"
  , "psci-support"
  , "pseudo-random"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
