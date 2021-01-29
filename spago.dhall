{ name = "my-project"
, dependencies = [ "aff", "mason-prelude", "simple-json" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
