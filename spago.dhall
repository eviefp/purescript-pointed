{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "pointed"
, license = "MIT"
, repository = "https://github.com/vladciobanu/purescript-pointed"
, dependencies =
  [ "bifunctors"
  , "foldable-traversable"
  , "generics-rep"
  , "lists"
  , "maybe"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
