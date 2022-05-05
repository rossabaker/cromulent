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
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    darwin.url = "github:LnL7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-darwin";

    # Extra community flakes
    devshell.url = "github:numtide/devshell";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    gomod2nix.url = "github:tweag/gomod2nix";

    # Emacs packages
    fill-sentences-correctly = {
      url = "github:duckwork/fill-sentences-correctly.el";
      flake = false;
    };

    hocon-mode.url = "github:jxq0/hocon-mode";
    hocon-mode.flake = false;

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
    , scala-mode
    , unmodified-buffer
    , utils
    , ...
    }@inputs:
    let
      tangle = pkgs: pkgs.stdenv.mkDerivation {
        name = "tangle";
        src = ./src/org/config/nix-darwin;
        buildInputs = [
          pkgs.emacs
        ];
        buildPhase = ''
          ${pkgs.emacs}/bin/emacs -Q -nw index.org --batch -f org-babel-tangle --kill
        '';
        installPhase = ''
          mkdir $out
          install * $out
        '';
      };

      tangleFor = system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        import (tangle pkgs);
    in
    {
      # Overlayed packages
      overlay = (import ./overlays);

      overlays = [
        emacs-overlay.overlay
        devshell.overlay
        self.overlay
      ];

      # System configurations
      # Accessible via 'nixos-rebuild --flake'
      nixosConfigurations = {
        # TODO: Replace with your hostname
        cool-computer = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";

          modules = [
            ./configuration.nix
            # Adds your overlay and packages to nixpkgs
            { nixpkgs.overlays = [ self.overlay ]; }
            # Adds your custom nixos modules
            ./modules/nixos
          ];
          # Pass our flake inputs into the config
          specialArgs = { inherit inputs; };
        };
      };

      # Home configurations
      # Accessible via 'home-manager --flake'
      homeConfigurations = {
        "ross.baker@C02Z721ZLVCG" = home-manager.lib.homeManagerConfiguration rec {
          username = "ross.baker";
          homeDirectory = "/Users/${username}";
          system = "x86_64-darwin";

          configuration = ./home.nix;
          extraModules = [
            # Adds your overlay and packages to nixpkgs
            { nixpkgs.overlays = [ self.overlay emacs-overlay.overlay gomod2nix.overlay ]; }
            # Adds your custom home-manager modules
            ./modules/work
          ];
          # Pass our flake inputs into the config
          extraSpecialArgs = { inherit inputs; };
        };
      };

      darwinConfigurations =
        let
          mkConfig = { system ? "x86_64-darwin" }: darwin.lib.darwinSystem {
            inherit system;
            modules = [
              (tangleFor system).darwin-configuration
            ];
          };
        in
        {
          C02Z721ZLVCG = mkConfig { };
        };
    }
    // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; overlays = self.overlays; };
      nix = pkgs.writeShellScriptBin "nix" ''
        exec ${pkgs.nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
      '';
      hm = home-manager.defaultPackage."${system}";
    in
    {
      # Your custom packages, plus nixpkgs and overlayed stuff
      # Accessible via 'nix build .#example' or 'nix build .#nixpkgs.example'
      packages = import ./pkgs { inherit pkgs; };

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
          pkgs.nix
          pkgs.terraform
        ];
      };
    });
}
