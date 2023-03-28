{
  description = "Ross A. Baker's perfectly cromulent Nix flake";
  inputs =
    {
      flake-parts.url = "github:hercules-ci/flake-parts";
    
      # Core nix flakes
      nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    
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
    
      on-el = {
        url = "gitlab:ajgrf/on.el";
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
    
      emacs-src.url = "github:emacs-mirror/emacs/emacs-29";
      emacs-src.flake = false;
    
      jinx = {
        url = "github:minad/jinx";
        flake = false;
      };
    };
  outputs =
    inputs:
      let
        mkDarwinConfig = { pkgs, system }:
          inputs.darwin.lib.darwinSystem {
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
          in
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
              homeModule
              inputs.self.homeManagerModules.emacs
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
          emacs = inputs.emacs-overlay.overlay;
          devshell = inputs.devshell.overlays.default;
        };
    
        pkgsFor = system: import inputs.nixpkgs {
          inherit system;
          overlays = builtins.attrValues inputs.self.overlays;
        };
      in
      inputs.flake-parts.lib.mkFlake { inherit inputs; } {
        imports = [
          ./gen/emacs
        ];
    
        flake = {
          inherit overlays;
    
          homeConfigurations = {
            "RABaker@L2LYQM57XY" = RABaker-at-L2LYQM57XY (pkgsFor "aarch64-darwin");
          };
    
          darwinConfigurations = {
            L2LYQM57XY = L2LYQM57XY (pkgsFor "aarch64-darwin");
          };
    
          flakeModules = {
            emacs = ./gen/emacs;
          };
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
                L2LYQM57XY = (L2LYQM57XY pkgs).system;
                "RABaker@L2LYQM57XY" = (RABaker-at-L2LYQM57XY pkgs).activationPackage;
              } else { };
          in
          {
            _module.args.pkgs = import inputs.nixpkgs {
              inherit system;
              overlays = [
                inputs.devshell.overlays.default
                inputs.emacs-overlay.overlays.default
              ];
            };
    
            packages = {
              website = pkgs.callPackage ./gen/website {
                emacs29 = self'.packages.emacs29;
                src = ./src;
              };
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
