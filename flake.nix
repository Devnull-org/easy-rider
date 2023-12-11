
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
    cardano-node.url = "github:input-output-hk/cardano-node/8.1.2";
    mithril.url = "github:input-output-hk/mithril/2337.0";
    hydra-node.url = "github:input-output-hk/hydra-node/0.13.0";
  };

  outputs =
    { self
    , flake-utils
    , nixpkgs
    , cardano-node
    , mithril
    , ...
    } @ inputs:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
    ]
      (system:
      let
        pkgs = import inputs.nixpkgs { inherit system; };

        easyRiderProject = import ./nix/project.nix {
          inherit (inputs) haskellNix iohk-nix CHaP ;
          inherit system nixpkgs;
        };

        easyRiderPackages = import ./nix/packages.nix {
          inherit easyRiderProject system pkgs cardano-node mithril;
        };

        easyRiderImages = import ./nix/docker.nix {
          inherit easyRiderPackages system nixpkgs;
        };

        prefixAttrs = s: attrs:
          with pkgs.lib.attrsets;
          mapAttrs' (name: value: nameValuePair (s + name) value) attrs;
      in
      rec {
        inherit easyRiderProject;

        packages =
          { default = easyRiderPackages.easy-rider; } //
          easyRiderPackages //
          prefixAttrs "docker-" easyRiderImages // {
            spec = import ./spec { inherit pkgs; };
          };

        devShells = (import ./nix/shell.nix {
          inherit (inputs) cardano-node mithril hydra-node;
          inherit easyRiderProject system;
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
