{ inputs, lib, moduleWithSystem, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay
  ];
  perSystem = { config, self', inputs', pkgs, system, ... }: {
    overlayAttrs = {
      inherit (config.packages) emacs29;
    };
    packages.emacs29 = pkgs.emacsGit.overrideAttrs (old: {
      name = "emacs29";
      # It's important this starts with the major number for Nix's
      # Emacs infra.  For example, it's used to blank out archaic
      # versions of the Seq package in MELPA.
      version = "29.0-${inputs.emacs-src.shortRev}";
      src = inputs.emacs-src;
    });
    packages.emacs-ross = pkgs.emacsWithPackagesFromUsePackage {
      package = config.packages.emacs29;
      override = epkgs: epkgs // {
        on = epkgs.trivialBuild {
          pname = "on.el";
          src = inputs.on-el;
        };
      };
      config = ./init.el;
      defaultInitFile = true;
      alwaysEnsure = false;
    };
    apps.emacs = {
      type = "app";
      program = "${config.packages.emacs-ross}/bin/emacs";
    };
  };
  flake = {
    homeManagerModules.emacs = moduleWithSystem (
      perSystem@{ config, pkgs }: {
        imports = [
          ({ pkgs, ...}: { home.packages = [ pkgs.ripgrep ]; })
          ({ pkgs, ...}: { home.packages = [ pkgs.gcc ]; })
        ];
        programs.emacs = {
          enable = true;
          package = config.packages.emacs-ross;
        };
      }
    );
  };
}
