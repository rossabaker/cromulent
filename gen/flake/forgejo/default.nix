{ ... }:
{
  flake.nixosModules.forgejo =
    { ... }:
    {
      imports = [
        ./nixos-module.nix
      ];
    };
}
