inputs @ { self, nixpkgs, nix-darwin, devshell, emacs-overlay, flake-parts, home-manager, ... }:

flake-parts.lib.mkFlake { inherit inputs; } (
  { withSystem, flake-parts-lib, ... }:

  let
    flakeModules = {
      homeModules = ./homeModules;
      darwinModules = ./darwinModules;
    };

    mkHomeConfig = { pkgs, system, username, homeDirectory }:
      home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          {
            home = {
              inherit homeDirectory username;
              stateVersion = "21.11";
            };
            nixpkgs.overlays = [ emacs-overlay.overlay ];
          }
          self.homeModules.base
          self.homeModules.emacs
          self.homeModules.scala
          self.homeModules.python
          ../../modules/work
        ];
        # Pass our flake inputs into the config
        extraSpecialArgs = { inherit inputs; };
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
      ./cachix
      ./garnix
      ./keyboard
      ./podman
      ./postgresql
      ../nix-darwin
      ../emacs
      ../scala
      ../python
      ../modern_ts
      ../home-manager
      flake-parts.flakeModules.easyOverlay
    ];

    flake = {
      inherit flakeModules overlays;

      homeConfigurations = {
        "RABaker@L2LYQM57XY" = mkHomeConfig {
          pkgs = (pkgsFor "aarch64-darwin");
          system = "aarch64-darwin";
          username = "RABaker";
          homeDirectory = "/Users/RABaker";
        };
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
