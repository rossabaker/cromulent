{ inputs, lib, flake-parts-lib, self, withSystem, ... }:

let
  inherit (flake-parts-lib) importApply;
in
{
  flake.nixosModules.opengist = importApply ./nixos-module.nix {
    inherit withSystem;
  };

  perSystem = { config, self', inputs', pkgs, system, ... }: {
    packages.opengist = pkgs.callPackage ./package.nix {
      src = inputs.opengist;
    };
  };
}
