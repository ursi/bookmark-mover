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
             let
               background = "background.js";

               manifest =
                 (p.formats.json {}).generate
                   "manifest.json"
                   (import ./manifest.nix { inherit background name; });

               name = "bookmark-mover";
             in
             p.runCommand name {}
               ''
               mkdir $out; cd $out
               ln -s ${manifest} manifest.json
               ln -s ${modules.Main.bundle {}} ${background}
               '';

           devShell =
             make-shell
               { packages =
                   with p;
                   [ nodejs
                     purs-nix.purescript
                     purs-nix.purescript-language-server
                     (command {})
                   ];
               };
         }
      )
      inputs;
}
