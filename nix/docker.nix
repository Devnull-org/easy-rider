{ easyRiderPackages 
, system ? builtins.currentSystem
, nixpkgs ? <nixpkgs>
}:
let
  pkgs = import nixpkgs { inherit system; };
in
{
  easy-rider = pkgs.dockerTools.buildImage {
    name = "easy-rider";
    tag = "latest";
    created = "now";
    config = {
      Entrypoint = [ "${easyRiderPackages.easy-rider}/bin/easy-rider-exe" ];
    };
  };

}
