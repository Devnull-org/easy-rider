{
  withoutDevTools ? false
, cardanoLabProject
, cardano-node
, system ? builtins.currentSystem
}:
let
  inherit (cardanoLabProject) compiler pkgs hsPkgs;

  cardano-node-pkgs = cardano-node.packages.${system};

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
    pkgs.pkgconfig
    cabal
    pkgs.haskellPackages.hspec-discover
    pkgs.haskellPackages.cabal-plan
    pkgs.python3Packages.jsonschema
    pkgs.plantuml
    cardano-node-pkgs.cardano-node
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
  ];

  haskellNixShell = hsPkgs.shellFor {
    packages = ps: with ps; [
      cardano-lab 
    ];

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
      pkgs.haskell.compiler.${compiler}
      pkgs.cabal-install
      pkgs.pkgconfig
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
      cardano-node-pkgs.cardano-node
      cardano-node-pkgs.cardano-cli
      hsPkgs.cardano-lab.components.exes.cardano-lab
    ];
  };
in
{
  default = haskellNixShell;
  cabalOnly = cabalShell;
  exes = exeShell;
}
