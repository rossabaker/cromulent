{
  description = "Ross A. Baker's perfectly cromulent Nix flake";

  outputs = inputs: import ./gen/flake inputs;

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Core nix flakes
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    # Home manager flake
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # nix-darwin flake
    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Extra community flakes
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";

    firefox-darwin.url = "github:bandithedoge/nixpkgs-firefox-darwin";
    firefox-darwin.inputs.nixpkgs.follows = "nixpkgs";

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Emacs packages
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

    scala-cli-repl = {
      url = "github:ag91/scala-cli-repl";
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
}
