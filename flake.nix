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
      devshell.inputs.nixpkgs.follows = "nixpkgs";
    
      emacs-overlay.url = "github:nix-community/emacs-overlay";
      emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    
      firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
      firefox-darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";
    
      # Emacs packages
      ammonite-term-repl = {
        url = "github:zwild/ammonite-term-repl";
        flake = false;
      };
    
      fill-sentences-correctly = {
        url = "github:duckwork/fill-sentences-correctly.el";
        flake = false;
      };
    
      git-related = {
        url = "git+https://codeberg.org/rossabaker/git-related";
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
    
      # Disabled pending license
      #   copilot-el = {
      #     url = "github:zerolfx/copilot.el";
      #     flake = false;
      #   };
    };
    outputs =
      inputs:
      let
        mkDarwinConfigModule = { pkgs }: {
          imports = [
            ./gen/nix-darwin
            {
              system.keyboard.enableKeyMapping = true;
              system.keyboard.remapCapsLockToControl = true;
            }
          ];
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
              inputs.self.homeManagerModules.base
              inputs.self.homeManagerModules.emacs
              inputs.self.homeManagerModules.scala
              inputs.self.homeManagerModules.python
              ./modules/work
            ];
            # Pass our flake inputs into the config
            extraSpecialArgs = { inherit inputs; };
          };
      
        aarch64-darwin-config-base = pkgs: mkDarwinConfigModule {
          inherit pkgs;
        };
      
        overlays = {
          emacs = inputs.emacs-overlay.overlay;
          devshell = inputs.devshell.overlays.default;
        };
      
        pkgsFor = system: import inputs.nixpkgs {
          inherit system;
          overlays = builtins.attrValues inputs.self.overlays;
        };
      
        darwinConfigurationModules = {
          aarch64-base = aarch64-darwin-config-base (pkgsFor "aarch64-darwin");
        };
      
        flakeModules = {
          emacs = ./gen/emacs;
          scala = ./gen/scala;
          python = ./gen/python;
          modernTs = ./gen/modern_ts;
          homeManager = ./gen/home-manager;
        };
      in
      inputs.flake-parts.lib.mkFlake { inherit inputs; } {
        imports = [
          ./gen/flake/modules/homeManagerModules.nix
          flakeModules.emacs
          flakeModules.scala
          flakeModules.python
          flakeModules.modernTs
          flakeModules.homeManager
          inputs.flake-parts.flakeModules.easyOverlay
        ];
      
        flake = {
          inherit overlays darwinConfigurationModules;
      
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
                  modules = [ darwinConfigurationModules.aarch64-base ];
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
      
              packages = {
                website = pkgs.callPackage ./gen/website {
                  emacs = self'.packages.emacs-ross;
                  src = ./src;
                };
      
                hyperlink = pkgs.callPackage ./src/nix/pkgs/hyperlink {};
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
      };
}
