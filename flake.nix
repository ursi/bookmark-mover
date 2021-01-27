{
  outputs = { self, nixpkgs, utils }:
    utils.simpleShell
      [
        "dhall"
        "gnumake"
        "nodejs"
        "purescript"
        "spago"
      ]
      nixpkgs;
}
