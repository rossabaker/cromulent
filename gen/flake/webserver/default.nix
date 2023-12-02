{ self, ... }:
{
  flake.nixosModules.webserver = {
    imports = [
      self.nixosModules.acme
      ./nixos-module.nix
    ];
  };
}
