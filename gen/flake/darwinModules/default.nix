# Adapted from https://github.com/hercules-ci/flake-parts/blob/7f53fdb7bdc5bb237da7fefef12d099e4fd611ca/modules/nixosModules.nix
# MIT License
# Copyright (c) 2021 Hercules CI

{ self, lib, flake-parts-lib, ... }:

let
  inherit (lib)
    mapAttrs
    mkOption
    types
  ;
  inherit (flake-parts-lib)
    mkSubmoduleOptions
  ;
in
{
  options = {
    flake = mkSubmoduleOptions {
      darwinModules = mkOption {
        type = types.lazyAttrsOf types.unspecified;
        default = { };
        apply = mapAttrs (k: v: { _file = "${toString self.outPath}/flake.nix#darwinModules.${k}"; imports = [ v ]; });
        description = ''
          Nix-Darwin modules.

          You may use this for reusable pieces of configuration, service modules, etc.
        '';
      };
    };
  };
}
