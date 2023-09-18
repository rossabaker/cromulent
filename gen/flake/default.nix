inputs:

inputs.flake-parts.lib.mkFlake { inherit inputs; } (
  { withSystem, flake-parts-lib, ... }:

  let
    inherit (flake-parts-lib) importApply;

    flakeModules = {
      homeModules = ./homeModules;
      darwinModules = ./darwinModules;
      garnix = ./garnix;
      podman = ./podman;
      postgresql = ./postgresql;
      nixDarwin = importApply ../nix-darwin { inherit (inputs) self; };
      emacs = ../emacs;
      scala = ../scala;
      python = ../python;
      modernTs = ../modern_ts;
      homeManager = ../home-manager;
      hyperlink = ./hyperlink;
    };

    mkHomeConfig = { pkgs, system, username, homeDirectory }:
      inputs.home-manager.lib.homeManagerConfiguration {
        inherit pkgs;
        modules = [
          {
            home = {
              inherit homeDirectory username;
              stateVersion = "21.11";
            };
            nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];
          }
          inputs.self.homeModules.base
          inputs.self.homeModules.emacs
          inputs.self.homeModules.scala
          inputs.self.homeModules.python
          ./modules/work
        ];
        # Pass our flake inputs into the config
        extraSpecialArgs = { inherit inputs; };
      };

    overlays = {
      emacs = inputs.emacs-overlay.overlay;
      devshell = inputs.devshell.overlays.default;
    };

    pkgsFor = system: import inputs.nixpkgs {
      inherit system;
      overlays = builtins.attrValues inputs.self.overlays;
    };
  in {
    imports = [
      flakeModules.homeModules
      flakeModules.darwinModules
      flakeModules.garnix
      flakeModules.podman
      flakeModules.postgresql
      flakeModules.nixDarwin
      flakeModules.emacs
      flakeModules.scala
      flakeModules.python
      flakeModules.modernTs
      flakeModules.homeManager
      flakeModules.hyperlink
      inputs.flake-parts.flakeModules.easyOverlay
    ];

    flake = {
      inherit overlays;

      homeConfigurations = {
        "RABaker@L2LYQM57XY" = mkHomeConfig {
          pkgs = (pkgsFor "aarch64-darwin");
          system = "aarch64-darwin";
          username = "RABaker";
          homeDirectory = "/Users/RABaker";
        };
      };

      inherit flakeModules;
    };

    systems = [
      "x86_64-linux"
      "aarch64-darwin"
    ];

    perSystem = { config, self', inputs', system, pkgs, ... }:
      let
        hm = inputs.home-manager.defaultPackage."${system}";

        darwinPackages =
          if (system == "aarch64-darwin") then {
            aarch64-darwin-config-base = (inputs.darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              modules = [ inputs.self.darwinModules.default ];
            }).system;
          } else { };
      in
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.devshell.overlays.default
              inputs.emacs-overlay.overlays.default
              (final: prev: {
                hyperlink = config.packages.hyperlink;
              })
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
              src = ./src;
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
            hyperlink = config.packages.hyperlink;
          };
        };
  }
)
