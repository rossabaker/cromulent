{ inputs, lib, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay
  ];
  perSystem = { config, self', inputs', pkgs, system, ... }: {
    _module.args.pkgs = import inputs.nixpkgs {
      inherit system;
      overlays = [ inputs.emacs-overlay.overlays.default ];
    };
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
    apps.emacs = {
      type = "app";
      program =
        let
          emacs = pkgs.emacsWithPackagesFromUsePackage {
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
        in "${emacs}/bin/emacs";
    };
  };
}
