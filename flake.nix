
{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:numtide/flake-utils";
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    cardano-node.url = "github:input-output-hk/cardano-node/1.35.7";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , cardano-node
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
    ]
      (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };
        cardanoLabProject = import ./nix/cardano-lab/project.nix {
          inherit (inputs) haskellNix iohk-nix CHaP;
          inherit system nixpkgs;
        };
        cardanoLabPackages = import ./nix/cardano-lab/packages.nix {
          inherit cardanoLabProject system pkgs cardano-node;
        };
        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
      in
      rec {
        inherit cardanoLabProject;

        packages = cardanoLabPackages;

        devShells = (import ./nix/cardano-lab/shell.nix {
          inherit (inputs) cardano-node;
          inherit cardanoLabProject system;
        }); 
      });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cardano-scaling.cachix.org"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = true;
  };
}
