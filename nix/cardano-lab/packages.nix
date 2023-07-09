{ cardanoLabProject
, system ? builtins.currentSystem
, pkgs
, cardano-node
}:
let
  nativePkgs = cardanoLabProject.hsPkgs;
  # Allow reinstallation of terminfo as it's not installed with cross compilers.
  patchedForCrossProject = cardanoLabProject.hsPkgs.appendModule
    ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "terminfo"; }; });
  musl64Pkgs = patchedForCrossProject.projectCross.musl64.hsPkgs;
in
rec {
  cardano-lab = nativePkgs.hydra-node.components.exes.cardano-lab;

  tests = {
    hydra-node = pkgs.mkShellNoCC {
      name = "cardano-lab-tests";
      buildInputs = [ nativePkgs.cardano-lab.components.tests.tests ];
    };
  };

  haddocks = pkgs.runCommand "cardano-lab-haddocks"
    {
      paths = [
        hydraProject.hsPkgs.cardano-lab.components.library.doc
        hydraProject.hsPkgs.hydra-lab.components.tests.tests.doc
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
