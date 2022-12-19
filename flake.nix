{
  description = "You new nix config";

  inputs = {
    # Utilities for building flakes
    utils.url = "github:numtide/flake-utils";

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
    gomod2nix.url = "github:tweag/gomod2nix";
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
    , gomod2nix
    , hocon-mode
    , home-manager
    , nixpkgs
    , ob-ammonite
    , scala-mode
    , unmodified-buffer
    , utils
    , ...
    }@inputs:
    let
      pkgsFor = system: import nixpkgs { inherit system; };

      mkDarwinConfig = { system }:
        let
          pkgs = pkgsFor system;
        in
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

      mkHomeConfig = { system, username, homeDirectory }:
        let
          pkgs = pkgsFor system;

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
              nixpkgs.overlays = [ emacs-overlay.overlay gomod2nix.overlays.default ];
            }
            homeModule
            emacsModule
            ./modules/work
          ];
          # Pass our flake inputs into the config
          extraSpecialArgs = { inherit inputs; };
        };

      RABaker-at-L2LYQM57XY = mkHomeConfig {
        system = "aarch64-darwin";
        username = "ross.baker";
        homeDirectory = "/Users/RABaker";
      };

      L2LYQM57XY = mkDarwinConfig {
        system = "aarch64-darwin";
      };
    in
    {
      overlays = {
        emacs = emacs-overlay.overlay;
        devshell = devshell.overlay;
      };

      # System configurations
      # Accessible via 'nixos-rebuild --flake'
      nixosConfigurations = {
        # cool-computer = nixpkgs.lib.nixosSystem {
        #   system = "x86_64-linux";

        #   modules = [
        #     ./configuration.nix
        #     # Adds your custom nixos modules
        #     ./modules/nixos
        #   ];
        #   # Pass our flake inputs into the config
        #   specialArgs = { inherit inputs; };
        # };
      };

      homeConfigurations = {
        "RABaker@L2LYQM57XY" = RABaker-at-L2LYQM57XY;
      };

      darwinConfigurations = {
        inherit L2LYQM57XY;
      };
    }
    // utils.lib.eachDefaultSystem (system:
    let

      pkgs = import nixpkgs { inherit system; overlays = builtins.attrValues self.overlays; };
      hm = home-manager.defaultPackage."${system}";
    in
    {
      # Your custom packages, plus nixpkgs and overlayed stuff
      # Accessible via 'nix build .#example' or 'nix build .#nixpkgs.example'
      packages = {
        website = pkgs.callPackage ./gen/website { src = ./src; };
      };

      # Devshell for bootstrapping plus editor utilities (fmt and LSP)
      # Accessible via 'nix develop'
      devShell = pkgs.devshell.mkShell {
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
    }) //
    {
      packages.aarch64-darwin = {
        L2LYQM57XY = L2LYQM57XY.system;
        "RABaker@L2LYQM57XY" = RABaker-at-L2LYQM57XY.activationPackage;
      };
    }
  ;
}
