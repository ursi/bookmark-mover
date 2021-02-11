# while true; do inotifywait -e modify -e move -e create -e delete . && make; done
{
  outputs = { self, nixpkgs, utils }:
    utils.mkShell
      ({ pkgs, ...} : with pkgs;
        {
          buildInputs = [
            dhall
            gnumake
            inotify-tools
            nodejs
            purescript
            spago
          ];

          shellHook =
            let
              inherit (nixpkgs) lib;

              watchedFiles = {
                copy = "$(addprefix $(src)/,$(copy))";
                purescript = "$(shell find src -type f)";
              };

              allWatchedFiles = lib.concatStringsSep " " (builtins.attrValues watchedFiles);

              makefile = writeText "Makefile"
                (builtins.replaceStrings [ "    " ] [ "\t" ]
                  ''
                  src = .
                  dist = dist

                  copy = *.json
                  copyTargets = $(addprefix $(dist)/,$(copy))

                  background = background.js

                  .PHONY: all watch

                  all : $(dist) $(copyTargets) $(dist)/$(background)

                  watch :
                      while true; do
                        make -f ${placeholder "out"}
                        inotifywait -e close_write -e move -e create -e delete ${allWatchedFiles}
                      done

                  $(dist) :
                      -mkdir $(dist)
                      touch $(dist)

                  removeSrc = $(1:$(src)/%=%)
                  makeAndLink = mkdir -p $(dist)/$(dir $1); ln -f $(src)/$1 $(dist)/$1;

                  $(copyTargets) : ${watchedFiles.copy}
                      $(call map,removeSrc makeAndLink,$?)

                  $(dist)/$(background) : ${watchedFiles.purescript}
                      spago bundle-app -t $@

                  clean :
                      rm -r $(dist)

                  # $(call compose,f1 f2 f3,w) -> $(call f3,$(call f2,$(call f1,w)))
                  compose = $(if $1,$(call compose,$(wordlist 2,$(words $1),$1),$(call $1,$2)),$2)

                  # $(call map,f1 f2,w1 w2) -> $(call compose,f1 f2,w1) $(call compose,f1 f2,w2)
                  map = $(foreach a,$2,$(call compose,$1,$a))

                  .ONESHELL :
                  ''
                );
            in
              ''
              alias make="make -f ${makefile}"
              '';
        }
      )
      nixpkgs;
}
