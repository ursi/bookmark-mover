{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils";
    };

  outputs = { nixpkgs, utils, purs-nix, ... }:
    utils.defaultSystems
      ({ pkgs, system }:
         let
           b = builtins; p = pkgs;
           inherit (purs-nix { inherit system; }) purs ps-pkgs ps-pkgs-ns;

           inherit
             (purs
                { dependencies =
                    with ps-pkgs-ns;
                    with ps-pkgs;
                    [ aff
                      event
                      simple-json
                      ursi.prelude
                    ];

                  src = ./src;
                }
             )
             modules
             shell;
         in
         { defaultPackage =
             p.runCommand "bookmark-mover" {}
               ''
               mkdir $out
               ln -s ${./manifest.json} $out/manifest.json
               ln -s ${modules.Main.bundle {}} $out/background.js
               '';

           devShell =
             with p;
             mkShell
               { buildInputs =
                   [ nodejs
                     purescript
                     (shell {})
                   ];
               };
         }
      )
      nixpkgs;
}
