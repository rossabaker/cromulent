{ ... }:
{
  flake.nixosModules.synapse =
    { ... }:
    {
      imports = [
        ./nixos-module.nix
      ];
    };
}
