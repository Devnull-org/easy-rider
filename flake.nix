
{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    crane.url = "github:ipetkov/crane";
    fenix.url = "github:nix-community/fenix";
    fenix.inputs.nixpkgs.follows = "nixpkgs";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    cardano-node.url = "github:IntersectMBO/cardano-node/10.5.1";
    cardano-node.flake = false; # otherwise, +2k dependencies we don’t really use
    mithril.url = "github:input-output-hk/mithril/2524.0";
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    cardano-playground.url = "github:input-output-hk/cardano-playground/56ebfef5595c43014029b039ade01b0ef06233e0";
    cardano-playground.flake = false; # otherwise, +9k dependencies in flake.lock…
    nix-bundle-exe.url = "github:3noch/nix-bundle-exe";
    nix-bundle-exe.flake = false;
  };

  outputs = inputs: let
    inherit (inputs.nixpkgs) lib;
  in
    inputs.flake-parts.lib.mkFlake {inherit inputs;} ({config, ...}: {
      imports = [
        inputs.devshell.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem = {system, ...}: let
        internal = inputs.self.internal.${system};
      in {
        packages =
          {
             default = internal.package;
             easy-rider = internal.package;

            inherit (internal) tx-build cardano-address;
          }
          // (lib.optionalAttrs (system == "x86_64-linux") {
            easy-rider-platform-x86_64-windows = inputs.self.internal.x86_64-windows.package;
          });

        devshells.default = import ./nix/devshells.nix {inherit inputs;};

        checks = internal.nixChecks;

      };

      flake = {
        internal =
          lib.genAttrs config.systems (
            targetSystem: import ./nix/internal/unix.nix {inherit inputs targetSystem;}
          )
          // lib.genAttrs ["x86_64-windows"] (
            targetSystem: import ./nix/internal/windows.nix {inherit inputs targetSystem;}
          );

      };
    });
}
