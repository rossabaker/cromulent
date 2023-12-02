{ inputs, self, withSystem, ... }:

{
  flake = withSystem "x86_64-linux" ( ctx@{ pkgs, system, ... }:
    {
      nixosConfigurations.abe =
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./configuration.nix
            self.nixosModules.netdata
            self.nixosModules.synapse
            self.nixosModules.forgejo
            self.nixosModules.webserver
            inputs.agenix.nixosModules.default
            {
              # TODO This is missing...
              nix.settings = {
                substituters = [ "https://cache.nixos.org/" ];
                trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
              };
            }
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
            {
              # TODO This is missing...
              nix.settings = {
                substituters = [ "https://cache.nixos.org/" ];
                trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
              };
            }
          ];
        };
    });
}
