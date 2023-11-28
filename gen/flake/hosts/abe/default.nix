{ inputs, self, withSystem, ... }:

{
  flake = withSystem "x86_64-linux" ( ctx@{ pkgs, system, ... }:
    {
      nixosConfigurations.abe =
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./configuration.nix
            inputs.agenix.nixosModules.default
          ];
        };

      homeConfigurations."ross@abe" =
        inputs.home-manager.lib.homeManagerConfiguration {
          inherit pkgs;
          modules = [
            self.homeModules.default
            {
              home.username = "ross";
              home.homeDirectory = "/home/ross";
            }
          ];
        };
    });
}
