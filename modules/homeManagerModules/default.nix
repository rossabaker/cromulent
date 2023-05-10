# Adapted from https://github.com/hercules-ci/flake-parts/blob/006c75898cf814ef9497252b022e91c946ba8e17/modules/nixosModules.nix
#
# It is needed so we can define flake.homeManagerModules in multiple flakeModules.

{ config, self, lib, flake-parts-lib, ... }:
let
  inherit (lib)
    filterAttrs
    mapAttrs
    mkOption
    optionalAttrs
    types
    ;
  inherit (flake-parts-lib)
    mkSubmoduleOptions
    ;
in
{
  options = {
    flake = mkSubmoduleOptions {
      homeManagerModules = mkOption {
        type = types.lazyAttrsOf types.unspecified;
        default = { };
        apply = mapAttrs (k: v: { _file = "${toString self.outPath}/flake.nix#homeManagerModules.${k}"; imports = [ v ]; });
      };
    };
  };
}
