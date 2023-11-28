{ inputs, ... }:

{
  flake.nixosConfigurations.abe =
    inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
        inputs.agenix.nixosModules.default
      ];
    };
}
