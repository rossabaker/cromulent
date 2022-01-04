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
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    gomod2nix.url = "github:tweag/gomod2nix";

    hocon-mode.url = "github:jxq0/hocon-mode";
    hocon-mode.flake = false;

    scala-mode = {
      url = "github:Kazark/emacs-scala-mode?ref=scala3";
      flake = false;
    };
  };

  outputs = { self,
              darwin,
              emacs-overlay,
              gomod2nix,
              hocon-mode,
              home-manager,
              nixpkgs,
              scala-mode,
              utils,
              ... }@inputs: {
    # Overlayed packages
    overlay = (import ./overlays);

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

    darwinConfigurations = {
      C02Z721ZLVCG = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
	modules = [ ./darwin-configuration.nix ];
      };
    };
  }
  // utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
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
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [ nix nixfmt rnix-lsp hm ];
      };
    });
}
