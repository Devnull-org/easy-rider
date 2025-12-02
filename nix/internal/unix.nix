
{
  inputs,
  targetSystem,
}:
# For now, let's keep all UNIX definitions together, until they diverge more in the future.
assert builtins.elem targetSystem ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"]; let
  buildSystem = targetSystem;
  pkgs = inputs.nixpkgs.legacyPackages.${buildSystem};
  inherit (pkgs) lib;
  extendForTarget = unix:
    (
      if pkgs.stdenv.isLinux
      then import ./linux.nix
      else if pkgs.stdenv.isDarwin
      then import ./darwin.nix
      else throw "can’t happen"
    ) {inherit inputs targetSystem unix;};
in
  extendForTarget rec {

    packageName = "easy-rider"; 

    commonArgs =
      {
        # inherit src;
        strictDeps = true;
        nativeBuildInputs = lib.optionals pkgs.stdenv.isLinux [
          pkgs.pkg-config
        ];
        buildInputs =
          lib.optionals pkgs.stdenv.isLinux [
            pkgs.openssl
          ]
          ++ lib.optionals pkgs.stdenv.isDarwin [
            pkgs.libiconv
            pkgs.darwin.apple_sdk_12_3.frameworks.SystemConfiguration
            pkgs.darwin.apple_sdk_12_3.frameworks.Security
            pkgs.darwin.apple_sdk_12_3.frameworks.CoreFoundation
          ];
      };

    nixChecks = {
      nix-statix =
        pkgs.runCommand "nix-statix" {
          buildInputs = [pkgs.statix];
        } ''
          touch $out
          cd ${inputs.self}
          exec statix check .
        '';

      nix-deadnix =
        pkgs.runCommand "nix-deadnix" {
          buildInputs = [pkgs.deadnix];
        } ''
          touch $out
          cd ${inputs.self}
          exec deadnix --fail .
        '';

      nix-nil =
        pkgs.runCommand "nix-nil" {
          buildInputs = [pkgs.nil];
        } ''
          ec=0
          touch $out
          cd ${inputs.self}
          find . -type f -iname '*.nix' | while IFS= read -r file; do
            nil diagnostics "$file" || ec=1
          done
          exit $ec
        '';

      # From `nixd`:
      nix-nixf =
        pkgs.runCommand "nix-nil" {
          buildInputs = [pkgs.nixf pkgs.jq];
        } ''
          ec=0
          touch $out
          cd ${inputs.self}
          find . -type f -iname '*.nix' | while IFS= read -r file; do
            errors=$(nixf-tidy --variable-lookup --pretty-print <"$file" | jq -c '.[]' | sed -r "s#^#$file: #")
            if [ -n "$errors" ] ; then
              cat <<<"$errors"
              echo
              ec=1
            fi
          done
          exit $ec
        '';
    };

    cardano-node-flake = let
      unpatched = inputs.cardano-node;
    in
      (import inputs.flake-compat {
        src =
          if targetSystem != "aarch64-darwin" && targetSystem != "aarch64-linux"
          then unpatched
          else {
            outPath = toString (pkgs.runCommand "source" {} ''
              cp -r ${unpatched} $out
              chmod -R +w $out
              cd $out
              echo ${lib.escapeShellArg (builtins.toJSON [targetSystem])} >$out/nix/supported-systems.nix
              ${lib.optionalString (targetSystem == "aarch64-linux") ''
                sed -r 's/"-fexternal-interpreter"//g' -i $out/nix/haskell.nix
              ''}
            '');
            inherit (unpatched) rev shortRev lastModified lastModifiedDate;
          };
      })
      .defaultNix;

    cardano-node-packages =
      {
        x86_64-linux = cardano-node-flake.hydraJobs.x86_64-linux.musl;
        inherit (cardano-node-flake.packages) x86_64-darwin aarch64-darwin aarch64-linux;
      }
      .${
        targetSystem
      };

    inherit (cardano-node-packages) cardano-node cardano-cli cardano-submit-api;

    cardano-node-configs-verbose = builtins.path {
      name = "cardano-playground-configs";
      path = inputs.cardano-playground + "/static/book.play.dev.cardano.org/environments";
    };

    cardano-node-configs =
      pkgs.runCommand "cardano-node-configs" {
        buildInputs = with pkgs; [jq];
      } ''
        cp -r ${cardano-node-configs-verbose} $out
        chmod -R +w $out
        find $out -name 'config.json' | while IFS= read -r configFile ; do
          jq '.
            | .TraceConnectionManager = false
            | .TracePeerSelection = false
            | .TracePeerSelectionActions = false
            | .TracePeerSelectionCounters = false
            | .TraceInboundGovernor = false
          ' "$configFile" >tmp.json
          mv tmp.json "$configFile"
        done
      '';

    generated-dir = pkgs.runCommand "generated-dir" {} ''
      mkdir -p $out
      ln -s ${cardano-node-configs} $out/cardano-node-configs
    '';

    stateDir =
      if pkgs.stdenv.isDarwin
      then "Library/Application Support/${packageName}"
      else ".local/share/${packageName}";

    runNode = network:
      pkgs.writeShellScriptBin "run-node-${network}" ''
        stateDir="$HOME"/${lib.escapeShellArg (stateDir + "/" + network)}
        mkdir -p "$stateDir"
        set -x
        exec ${lib.getExe cardano-node} run \
          --config ${cardano-node-configs}/${network}/config.json \
          --topology ${cardano-node-configs}/${network}/topology.json \
          --socket-path "$stateDir"/node.socket \
          --database-path "$stateDir"/chain
      ''
      // {meta.description = "Runs cardano-node on ${network}";};

    # For generating a signing key from a recovery phrase. It’s a little
    # controversial to download a binary, but we only need it for the devshell. If
    # needed, we can use the source instead.
    cardano-address =
      if targetSystem == "aarch64-linux"
      then
        pkgs.writeShellApplication {
          name = "cardano-address";
          text = ''
            echo >&2 "TODO: unimplemented: compile \`cardano-address\` for \`${targetSystem}\`!"
            exit 1
          '';
        }
      else let
        release = "v2024-09-29";
        baseUrl = "https://github.com/cardano-foundation/cardano-wallet/releases/download/${release}/cardano-wallet";
        archive = pkgs.fetchzip {
          name = "cardano-wallet-${release}";
          url =
            {
              "x86_64-linux" = "${baseUrl}-${release}-linux64.tar.gz";
              "x86_64-darwin" = "${baseUrl}-${release}-macos-intel.tar.gz";
              "aarch64-darwin" = "${baseUrl}-${release}-macos-silicon.tar.gz";
            }
            .${
              targetSystem
            };
          hash =
            {
              "x86_64-linux" = "sha256-EOe6ooqvSGylJMJnWbqDrUIVYzwTCw5Up/vU/gPK6tE=";
              "x86_64-darwin" = "sha256-POUj3Loo8o7lBI4CniaA/Z9mTRAmWv9VWAdtcIMe27I=";
              "aarch64-darwin" = "sha256-+6bzdUXnJ+nnYdZuhLueT0+bYmXzwDXTe9JqWrWnfe4=";
            }
            .${
              targetSystem
            };
        };
      in
        pkgs.runCommand "cardano-address" {
          meta.description = "Command-line for address and key manipulation in Cardano";
        } ''
          mkdir -p $out/bin $out/libexec
          cp ${archive}/cardano-address $out/libexec/
          ${lib.optionalString pkgs.stdenv.isDarwin ''
            cp ${archive}/{libz,libiconv.2,libgmp.10,libffi.8}.dylib $out/libexec
          ''}
          ln -sf $out/libexec/cardano-address $out/bin/
        '';


    mithril-client = inputs.mithril.packages.${targetSystem}.mithril-client-cli;

    mithrilGenesisVerificationKeys = {
      preview = builtins.readFile (inputs.mithril + "/mithril-infra/configuration/pre-release-preview/genesis.vkey");
      preprod = builtins.readFile (inputs.mithril + "/mithril-infra/configuration/release-preprod/genesis.vkey");
      mainnet = builtins.readFile (inputs.mithril + "/mithril-infra/configuration/release-mainnet/genesis.vkey");
    };

    mithrilAncillaryVerificationKeys = {
      preview = builtins.readFile (inputs.mithril + "/mithril-infra/configuration/pre-release-preview/ancillary.vkey");
      preprod = builtins.readFile (inputs.mithril + "/mithril-infra/configuration/release-preprod/ancillary.vkey");
      mainnet = builtins.readFile (inputs.mithril + "/mithril-infra/configuration/release-mainnet/ancillary.vkey");
    };

    mithrilAggregator = {
      preview = "https://aggregator.pre-release-preview.api.mithril.network/aggregator";
      preprod = "https://aggregator.release-preprod.api.mithril.network/aggregator";
      mainnet = "https://aggregator.release-mainnet.api.mithril.network/aggregator";
    };

  }
