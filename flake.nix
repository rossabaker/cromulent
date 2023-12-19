{
  description = "Ross A. Baker's perfectly cromulent Nix flake";

  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-src = {
      url = "github:emacs-mirror/emacs/emacs-29";
      flake = false;
    };

    firefox-darwin = {
      url = "github:bandithedoge/nixpkgs-firefox-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
    };

    git-related = {
      url = "git+https://codeberg.org/rossabaker/git-related";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    jinx = {
      url = "github:minad/jinx";
      flake = false;
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-dns = {
      url = "github:Janik-Haag/NixOS-DNS";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    on-el = {
      url = "gitlab:ajgrf/on.el";
      flake = false;
    };

    scala-cli-repl = {
      url = "github:ag91/scala-cli-repl";
      flake = false;
    };
  };

  outputs = inputs: import ./gen/flake inputs;
}
