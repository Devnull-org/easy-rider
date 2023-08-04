{ compiler ? "ghc928"
, system ? builtins.currentSystem
, haskellNix
, iohk-nix
, CHaP
, nixpkgs ? iohk-nix.nixpkgs
}:
let
  pkgs = import nixpkgs {
    inherit system;
    overlays = [
      haskellNix.overlay
      iohk-nix.overlays.crypto
    ];
  };

  hsPkgs = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanSourceWith {
      name = "cardano-lab";
      src = ./../..;
      filter = path: type:
        builtins.all (x: baseNameOf path != x) [
          "flake.nix"
          "flake.lock"
          "nix"
          ".github"
        ];
    };
    projectFileName = "cabal.project";
    compiler-nix-name = compiler;

    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

    modules = [
      {
        packages.cardano-lab.dontStrip = false;
      }
      ({ pkgs, lib, ... }:
        {
          packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf pkgs.secp256k1 ] ];
          packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
        }
      )
    ];
  };
in
{
  inherit compiler pkgs hsPkgs;
}
