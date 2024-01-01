{ inputs, self, withSystem, ... }:

{
  flake = withSystem "x86_64-linux" ( ctx@{ pkgs, system, ... }:
    {
      nixosConfigurations.abe =
        inputs.nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            ./configuration.nix
            inputs.nixos-dns.nixosModules.dns
            ./dns
            self.nixosModules.netdata
            self.nixosModules.synapse
            self.nixosModules.forgejo
            self.nixosModules.opengist
            self.nixosModules.acme
            self.nixosModules.webserver
            self.nixosModules.libravatar
            inputs.agenix.nixosModules.default
            {
              # TODO This is missing...
              nix.settings = {
                substituters = [ "https://cache.nixos.org/" ];
                trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];
              };

              cromulent.services.opengist = {
                enabled = true;
                domain = "paste.rossabaker.com";
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
            {
              # Sloppy fix for now
              nix = {
                extraOptions = ''
                  experimental-features = nix-command flakes
                '';
              };
            }
          ];
        };
    });
}
