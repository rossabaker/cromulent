inputs @ { self, nixpkgs, nix-darwin, devshell, emacs-overlay, flake-parts, home-manager, agenix, ... }:

flake-parts.lib.mkFlake { inherit inputs; } (
  { withSystem, flake-parts-lib, ... }:

  let
    flakeModules = {
      homeModules = ./homeModules;
      darwinModules = ./darwinModules;
    };

    overlays = {
      emacs = emacs-overlay.overlay;
      devshell = devshell.overlays.default;
    };

    pkgsFor = system: import nixpkgs {
      inherit system;
      overlays = builtins.attrValues self.overlays;
    };
  in {
    imports = [
      flakeModules.homeModules
      flakeModules.darwinModules
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
      ./podman
      ./postgresql
      ./modern_ts
      flake-parts.flakeModules.easyOverlay
    ];

    flake = {
      inherit flakeModules overlays;

      nixosConfigurations.abe = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./nixos/abe/configuration.nix
          agenix.nixosModules.default
        ];
      };

      homeModules.default = {
        imports = builtins.attrValues (builtins.removeAttrs self.homeModules [ "default" ]);
        home.stateVersion = "21.11";
        nixpkgs.overlays = [ emacs-overlay.overlay ];
      };

      homeConfigurations."ross@abe" =
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          modules = [
            self.homeModules.default
            {
              home.username = "ross";
              home.homeDirectory = "/home/ross";
            }
          ];
        };
    };

    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    perSystem = { config, self', inputs', system, pkgs, ... }:
      {
        _module.args.pkgs = import nixpkgs {
          inherit system;
          overlays = [
            devshell.overlays.default
            emacs-overlay.overlays.default
          ];
        };

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

        devShells.default = pkgs.devshell.mkShell {
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
            pkgs.nix
          ];
        };
      };
  }
)
