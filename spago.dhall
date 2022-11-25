{ name = "classless-encode-json"
, dependencies =
  [ "argonaut"
  , "argonaut-codecs"
  , "argonaut-core"
  , "classless"
  , "either"
  , "foreign-object"
  , "maybe"
  , "partial"
  , "prelude"
  , "record"
  , "tuples"
  , "type-equality"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
