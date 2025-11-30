
{inputs}: {
  config,
  pkgs,
  ...
}: let
  inherit (pkgs) lib;
  internal = inputs.self.internal.${pkgs.system};
in {
  name = "build-on-cardano";

  imports = [
    "${inputs.devshell}/extra/language/c.nix"
    "${inputs.devshell}/extra/language/rust.nix"
  ];

  commands = [
    {
      name = "cardano-node";
      package = internal.cardano-node;
    }
    {
      name = "cardano-cli";
      package = internal.cardano-cli;
    }
    {
      name = "cardano-submit-api";
      package = internal.cardano-submit-api;
    }
    {
      name = "cardano-address";
      package = internal.cardano-address;
    }
    {
      category = "handy";
      package = internal.runNode "preview";
    }
    {
      category = "handy";
      package = internal.runNode "preprod";
    }
    {
      category = "handy";
      package = internal.runNode "mainnet";
    }
    {
      category = "handy";
      package = internal.runDolos "preview";
    }
    {
      category = "handy";
      package = internal.runDolos "preprod";
    }
    {
      category = "handy";
      package = internal.runDolos "mainnet";
    }
  ];

  language.c = {
    compiler =
      if pkgs.stdenv.isLinux
      then pkgs.gcc
      else pkgs.clang;
    includes = internal.commonArgs.buildInputs;
  };

  language.rust = {
    packageSet = internal.rustPackages;
    tools = ["cargo" "rustfmt"]; # The rest is provided below.
    enableDefaultToolchain = true;
  };

  env =
    [
    ]
    ++ lib.optionals pkgs.stdenv.isDarwin [
      {
        name = "LIBCLANG_PATH";
        value = internal.commonArgs.LIBCLANG_PATH;
      }
    ]
    ++ lib.optionals pkgs.stdenv.isLinux [
      # Embed `openssl` in `RPATH`:
      {
        name = "RUSTFLAGS";
        eval = ''"-C link-arg=-Wl,-rpath,$(pkg-config --variable=libdir openssl)"'';
      }
    ];

  devshell = {
    packages =
      [
        pkgs.unixtools.xxd
        pkgs.websocat
      ]
      ++ lib.optionals pkgs.stdenv.isLinux [
        pkgs.pkg-config
      ]
      ++ lib.optionals pkgs.stdenv.isDarwin [
        pkgs.libiconv
      ];

    motd = ''

      {202}🔨 Welcome to ${config.name}{reset}
      $(menu)

    '';

    startup.symlink-configs.text = ''
      for old_link in cardano-node-configs dolos-configs ; do
        if [[ -L "$PRJ_ROOT/$old_link" ]] ; then rm -- "$PRJ_ROOT/$old_link" ; fi
      done

      ln -sfn ${internal.generated-dir} "$PRJ_ROOT/generated"
    '';
  };
}

