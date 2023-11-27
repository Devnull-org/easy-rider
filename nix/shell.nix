{
  withoutDevTools ? false
, easyRiderProject
, cardano-node
, mithril
, hydra-node
, system ? builtins.currentSystem
}:
let
  inherit (easyRiderProject) compiler pkgs hsPkgs;

  cardano-node-pkgs = cardano-node.packages.${system};
  mithril-pkgs = mithril.packages.${system};
  hydra-node-pkgs = hydra-node.packages.${system};

  cabal = pkgs.haskell-nix.cabal-install.${compiler};

  haskell-language-server = pkgs.haskell-nix.tool compiler "haskell-language-server" rec {
    src = pkgs.haskell-nix.sources."hls-1.10";
    cabalProject = builtins.readFile (src + "/cabal.project");
    sha256map."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
  };

  libs = [
    pkgs.glibcLocales
    pkgs.libsodium-vrf # from iohk-nix overlay
    pkgs.lzma
    pkgs.secp256k1
    pkgs.zlib
  ]
  ++
  pkgs.lib.optionals (pkgs.stdenv.isLinux) [ pkgs.systemd ];

  buildInputs = [
    pkgs.git
    pkgs.pkg-config
    cabal
    (pkgs.haskell-nix.tool compiler "cabal-plan" "latest")
    pkgs.haskellPackages.hspec-discover
    # Formatting
    pkgs.treefmt
    (pkgs.haskell-nix.tool compiler "fourmolu" "0.13.0.0")
    (pkgs.haskell-nix.tool compiler "cabal-fmt" "0.1.7")
    pkgs.nixpkgs-fmt
    # For validating JSON instances against a pre-defined schema
    pkgs.check-jsonschema

    cardano-node-pkgs.cardano-node
    mithril-pkgs.mithril-client
    hydra-node-pkgs.hydra-node
  ];

  devInputs = if withoutDevTools then [ ] else [
    pkgs.haskellPackages.fourmolu
    pkgs.haskellPackages.cabal-fmt
    haskell-language-server
    pkgs.haskellPackages.ghcid
    pkgs.haskellPackages.graphmod
    pkgs.websocat
    pkgs.yq
    cardano-node-pkgs.cardano-cli
    mithril-pkgs.mithril-client
    hydra-node-pkgs.hydra-node
  ];

  haskellNixShell = hsPkgs.shellFor {
    # packages = ps: with ps; [
    #   easy-rider 
    # ];

    buildInputs = libs ++ buildInputs ++ devInputs;

    withHoogle = !withoutDevTools;

    # Always create missing golden files
    CREATE_MISSING_GOLDEN = 1;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
  };

  # A "cabal-only" shell which does not use haskell.nix
  cabalShell = pkgs.mkShell {
    name = "cabal-shell";

    buildInputs = libs ++ [
      hsPkgs.easy-rider.components.exes.easy-rider
      pkgs.haskell-nix.compiler.${compiler}
      pkgs.cabal-install
      pkgs.pkg-config
    ] ++ buildInputs ++ devInputs;

    # Ensure that libz.so and other libraries are available to TH splices.
    LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libs;

    # Force a UTF-8 locale because many Haskell programs and tests
    # assume this.
    LANG = "en_US.UTF-8";

    # Make the shell suitable for the stack nix integration
    # <nixpkgs/pkgs/development/haskell-modules/generic-stack-builder.nix>
    GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    STACK_IN_NIX_SHELL = "true";
  };

  exeShell = pkgs.mkShell {
    name = "exe-shell";

    buildInputs = [
      hsPkgs.easy-rider.components.exes.easy-rider
      cardano-node-pkgs.cardano-node
      mithril-pkgs.mithril-client
      hydra-node-pkgs.hydra-node
    ];
  };
in
{
  default = haskellNixShell;
  cabalOnly = cabalShell;
  exes = exeShell;
}
