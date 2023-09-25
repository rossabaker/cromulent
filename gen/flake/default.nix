inputs @ { self, nixpkgs, nix-darwin, devshell, emacs-overlay, flake-parts, home-manager, ... }:

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
      ./garnix
      ./cachix
      ./scala
      ./python
      ./home-manager
      ./nix-darwin
      ./keyboard
      ./podman
      ./postgresql
      ./modern_ts
      flake-parts.flakeModules.easyOverlay
    ];

    flake = {
      inherit flakeModules overlays;

      homeModules.default = {
        imports = [
          self.homeModules.base
          self.homeModules.emacs
          self.homeModules.scala
          self.homeModules.python
          ../../modules/work
        ];
        home.stateVersion = "21.11";
        nixpkgs.overlays = [ emacs-overlay.overlay ];
      };

      homeConfigurations.aarch64-darwin-example = {
        extraSpecialArgs = { inherit inputs; };
        pkgs = import nixpkgs {
          system = "aarch64-darwin";
        };
        modules = [
          self.homeModules.default
          {
            home = rec {
              username = "Example";
              homeDirectory = "/Users/${username}";
            };
          }
        ];
      };
    };

    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    perSystem = { config, self', inputs', system, pkgs, ... }:
      let
        hm = home-manager.defaultPackage."${system}";

        darwinPackages =
          if (system == "aarch64-darwin") then {
            aarch64-darwin-config-base = (nix-darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              modules = [ self.darwinModules.default ];
            }).system;
          } else { };
      in
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
            website = pkgs.callPackage ../website {
              emacs = self'.packages.emacs-ross;
              src = ../../src;
            };
          } // darwinPackages;

          devShells.default = pkgs.devshell.mkShell {
            name = "cromulent";

            commands = [
              {
                name = "hm-switch";
                help = "switch the home-manager config";
                command = "${hm}/bin/home-manager switch --flake $PRJ_ROOT";
              }
              {
                name = "serve";
                help = "run 'hugo serve' on the local project";
                command = "(cd $PRJ_ROOT && ${pkgs.hugo}/bin/hugo serve --disableFastRender --config tmp/hugo/config.toml)";
              }
            ];

            packages = [
              hm
              pkgs.google-cloud-sdk
              pkgs.hugo
              pkgs.nix
              pkgs.terraform
            ];
          };

          overlayAttrs = {
          };
        };
  }
)
