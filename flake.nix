{ inputs =
    { nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
      purs-nix.url = "github:ursi/purs-nix";
      utils.url = "github:ursi/flake-utils/2";
    };

  outputs = { utils, ... }@inputs:
    utils.default-systems
      ({ make-shell, pkgs, purs-nix, ... }:
         let
           b = builtins; p = pkgs;
           inherit (purs-nix) purs ps-pkgs ps-pkgs-ns;

           inherit
             (purs
                { dependencies =
                    with ps-pkgs-ns;
                    with ps-pkgs;
                    [ aff
                      event
                      ursi.prelude
                      ursi.simple-json
                    ];

                  srcs = [ ./src ];
                }
             )
             modules
             command;
         in
         { defaultPackage =
             p.runCommand "bookmark-mover" {}
               ''
               mkdir $out
               ln -s ${./manifest.json} $out/manifest.json
               ln -s ${modules.Main.bundle {}} $out/background.js
               '';

           devShell =
             make-shell
               { packages =
                   with p;
                   [ nodejs
                     purs-nix.purescript
                     (command {})
                   ];
               };
         }
      )
      inputs;
}
