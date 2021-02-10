{ name = "my-project"
, dependencies = [ "aff", "event", "mason-prelude", "simple-json" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
