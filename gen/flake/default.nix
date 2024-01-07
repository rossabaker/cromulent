inputs @ { self, nixpkgs, flake-parts, emacs-overlay, devshell, ... }:

flake-parts.lib.mkFlake { inherit inputs; } (
  { withSystem, flake-parts-lib, ... }:

  {
    imports = [
      inputs.devshell.flakeModule
      ./homeModules
      ./darwinModules
      ./emacs
      ./binaryCaches
      ./garnix
      ./cachix
      ./scala
      ./python
      ./home-manager
      ./home-manager/darwin.nix
      ./nix-darwin
      ./keyboard
      ./podman/nixos.nix
      ./podman/nix-darwin.nix
      ./postgresql
      # ./modern_ts
      ./hosts/abe
      ./netdata
      ./forgejo
      ./forgejoActions
      ./opengist
      ./webserver
      ./acme
      ./dns
      ./synapse
      ./libravatar
    ];

    flake = {
      homeModules.default = {
        imports = builtins.attrValues (builtins.removeAttrs self.homeModules [ "default" ]);
        home.stateVersion = "21.11";
        nixpkgs.overlays = [ emacs-overlay.overlay ];
      };
    };

    systems = [
      "aarch64-darwin"
      "aarch64-linux"
      "x86_64-darwin"
      "x86_64-linux"
    ];

    perSystem = { config, self', inputs', system, pkgs, ... }:
      {
        apps = {
          hello = {
            type = "app";
            program = "${pkgs.hello}/bin/hello";
          };
        };

        packages = {
          website = pkgs.callPackage ../pkgs/website {
            emacs = self'.packages.emacs-ross;
            src = ../../src;
          };
        };

        checks = {
          inherit (self'.packages) website;
        };

        devshells.cromulent = {
          name = "cromulent";

          commands = [
            {
              name = "serve";
              help = "run 'hugo serve' on the local project";
              command = "(cd $PRJ_ROOT && ${pkgs.hugo}/bin/hugo serve --disableFastRender --config tmp/hugo/config.toml)";
            }
          ];

          packages = [
            pkgs.hugo
            (pkgs.octodns.withProviders (ps:
              with pkgs.octodns-providers; [
                bind
                hetzner
              ]
            ))
          ];
        };
      };
  }
)
