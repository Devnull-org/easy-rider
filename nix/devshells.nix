
{inputs}: {
  config,
  pkgs,
  ...
}: let
  inherit (pkgs) lib;
  internal = inputs.self.internal.${pkgs.system};
in {
  name = "easy-rider";

  imports = [
    "${inputs.devshell}/extra/language/c.nix"
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
  ];

  language.c = {
    compiler =
      if pkgs.stdenv.isLinux
      then pkgs.gcc
      else pkgs.clang;
    includes = internal.commonArgs.buildInputs;
  };

  env = [];

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
      ln -sfn ${internal.generated-dir} "$PRJ_ROOT/generated"
    '';
  };
}

