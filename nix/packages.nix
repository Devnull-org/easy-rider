{ easyRiderProject
, system ? builtins.currentSystem
, pkgs
, cardano-node
, mithril 
}:
let
  # Allow reinstallation of terminfo as it's not installed with cross compilers.
  patchedForCrossProject = easyRiderProject.hsPkgs.appendModule
    ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
  musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;
in
rec {
  easy-rider = easyRiderProject.hsPkgs.easy-rider.components.exes.easy-rider-exe;

  easy-rider-static = musl64Pkgs.easy-rider.components.exes.easy-rider-exe;

  tests = {
    easy-rider-tests = pkgs.mkShellNoCC {
      name = "easy-rider-tests";
      buildInputs = [ easyRiderProject.hsPkgs.easy-rider.components.tests.tests ];
    };
  };

  haddocks = pkgs.runCommand "easy-rider-haddocks"
    {
      paths = [
        easyRiderProject.hsPkgs.easy-rider.components.library.doc
        easyRiderProject.hsPkgs.easy-rider.components.tests.tests.doc
      ];
    }
    ''
      set -ex
      mkdir -p $out
      for p in $paths; do
        cd $p
        for html in $(find $haddockRoot -name html -type d); do
          package=$(basename $(dirname $html))
          mkdir -p $out/$package
          cp -a $html/* $out/$package/
        done
      done
    '';
}
