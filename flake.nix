{
  description = "You new nix config";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Core nix flakes
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    hardware.url = "github:nixos/nixos-hardware";

    # Home manager flake
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # nix-darwin flake
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-22.11-darwin";
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";

    # Extra community flakes
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";

    # Emacs packages
    ammonite-term-repl = {
      url = "github:zwild/ammonite-term-repl";
      flake = false;
    };

    fill-sentences-correctly = {
      url = "github:duckwork/fill-sentences-correctly.el";
      flake = false;
    };

    hocon-mode = {
      url = "github:jxq0/hocon-mode";
      flake = false;
    };

    ob-ammonite = {
      url = "github:zwild/ob-ammonite";
      flake = false;
    };

    scala-mode = {
      url = "github:Kazark/emacs-scala-mode?ref=scala3";
      flake = false;
    };

    unmodified-buffer = {
      url = "github:arthurcgusmao/unmodified-buffer";
      flake = false;
    };
  };

  outputs =
    { self
    , darwin
    , devshell
    , emacs-overlay
    , fill-sentences-correctly
    , hocon-mode
    , home-manager
    , nixpkgs
    , ob-ammonite
    , scala-mode
    , unmodified-buffer
    , flake-parts
    , ...
    }@inputs:
    let
      mkDarwinConfig = { pkgs, system }:
        darwin.lib.darwinSystem {
          inherit system;
          modules = [
            (import (pkgs.callPackage ./tangle.nix {
              inherit pkgs;
              src = ./src/org/config/nix-darwin;
            }))
            {
              system.keyboard.enableKeyMapping = true;
              system.keyboard.remapCapsLockToControl = true;
            }
          ];
        };

      mkHomeConfig = { pkgs, system, username, homeDirectory }:
        let
          homeModule = import (pkgs.callPackage ./tangle.nix {
            inherit pkgs;
            src = ./src/org/config/home-manager;
          });

          emacsModule = import (pkgs.callPackage ./tangle.nix {
            inherit pkgs;
            src = ./src/org/config/emacs;
          });
        in
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
            homeModule
            emacsModule
            ./modules/work
          ];
          # Pass our flake inputs into the config
          extraSpecialArgs = { inherit inputs; };
        };

      RABaker-at-L2LYQM57XY = pkgs: mkHomeConfig {
        inherit pkgs;
        system = "aarch64-darwin";
        username = "ross.baker";
        homeDirectory = "/Users/RABaker";
      };

      L2LYQM57XY = pkgs: mkDarwinConfig {
        inherit pkgs;
        system = "aarch64-darwin";
      };

      overlays = {
        emacs = emacs-overlay.overlay;
        devshell = devshell.overlay;
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        inherit overlays;

        homeConfigurations = { pkgs, ... }: {
          "RABaker@L2LYQM57XY" = RABaker-at-L2LYQM57XY pkgs;
        };

        darwinConfigurations = { pkgs, ... }: {
          inherit L2LYQM57XY pkgs;
        };
      };

      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = { config, self', inputs', system, ... }:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = builtins.attrValues self.overlays;
          };
          hm = home-manager.defaultPackage."${system}";

          darwinPackages =
            if (system == "aarch64-darwin") then {
              L2LYQM57XY = (L2LYQM57XY pkgs).system;
              "RABaker@L2LYQM57XY" = (RABaker-at-L2LYQM57XY pkgs).activationPackage;
            } else { };
        in
        {
          packages = {
            website = pkgs.callPackage ./gen/website { src = ./src; };
          } // darwinPackages;

          devShells.default = pkgs.devshell.mkShell {
            name = "nix-config";

            commands = [{
              name = "hm-switch";
              help = "switch the home-manager config";
              command = "${hm}/bin/home-manager switch --flake $PRJ_ROOT";
            }];

            packages = [
              hm
              pkgs.google-cloud-sdk
              pkgs.hugo
              pkgs.nix
              pkgs.terraform
            ];
          };
        };
    };
}
