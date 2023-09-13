{ inputs, lib, moduleWithSystem, ... }: {
  imports = [
    inputs.flake-parts.flakeModules.easyOverlay
  ];
  perSystem = { config, self', inputs', pkgs, system, ... }: {
    overlayAttrs = {
      inherit (config.packages) emacs29;
    };
    packages.emacs29 = pkgs.emacs-git.overrideAttrs (old: {
      name = "emacs29";
      # It's important this starts with the major number for Nix's
      # Emacs infra.  For example, it's used to blank out archaic
      # versions of the Seq package in MELPA.
      version = "29.0-${inputs.emacs-src.shortRev}";
      src = inputs.emacs-src;
      # This doesn't apply to Emacs29.
      patches = builtins.filter (p: baseNameOf p != "bytecomp-revert.patch") old.patches;
    });
    packages.emacs-ross = pkgs.emacsWithPackagesFromUsePackage {
      package = config.packages.emacs29;
      override = epkgs: epkgs // {
        on = epkgs.trivialBuild {
          pname = "on.el";
          src = inputs.on-el;
          version = "0.1.0";
        };
        jinx =
          let
            jinx-lisp = epkgs.trivialBuild {
              pname = "jinx-lisp";
              src = inputs.jinx;
              packageRequires = [ epkgs.compat ];
              version = "0.9";
            };
            jinx-mod = pkgs.stdenv.mkDerivation {
              name = "jinx-mod";
              src = inputs.jinx;
              buildInputs = [ pkgs.enchant2 ];
              buildPhase = ''
                cc -I. -O2 -Wall -Wextra -fPIC -shared -o jinx-mod.dylib jinx-mod.c \
                  -I${pkgs.enchant2.dev}/include/enchant-2 -lenchant-2
              '';
              installPhase = ''
                LISPDIR=$out/share/emacs/site-lisp
                install -d $LISPDIR
                install *.dylib $LISPDIR
              '';
            };
          in
          pkgs.symlinkJoin {
            name = "jinx";
            paths = [ jinx-lisp jinx-mod ];
          };
        copilot =
          let
            copilot-lisp = epkgs.trivialBuild {
              pname = "copilot-lisp";
              src = inputs.copilot-el;
              packageRequires = [
                epkgs.dash
                epkgs.editorconfig
                epkgs.s
              ];
              version = "0.0.0";
            };
            copilot-dist = pkgs.stdenv.mkDerivation {
              name = "copilot-dist";
              src = inputs.copilot-el;
              installPhase = ''
                LISPDIR=$out/share/emacs/site-lisp
                mkdir -p $LISPDIR
                cp -R dist $LISPDIR
              '';
            };
          in
          pkgs.symlinkJoin {
            name = "copilot.el";
            paths = [ copilot-lisp copilot-dist ];
          };
        ox-slack =
          epkgs.ox-slack.overrideAttrs(old: {
            patches = [ ../../src/emacs/ox-slack/github-9.patch ];
          });
        git-related =
          epkgs.trivialBuild {
            pname = "git-related";
            src = inputs.git-related;
            version = "1.1";
          };
      };
      config = ./init.el;
      defaultInitFile = true;
      alwaysEnsure = false;
      extraEmacsPackages = epkgs: [
        (epkgs.trivialBuild {
          pname = "rab-lisp";
          src = ./lisp/rab-lisp.el;
          version = "0.1.0";
        })
        (epkgs.trivialBuild {
          pname = "rab-ox-hugo";
          src = ./lisp/rab-ox-hugo.el;
          packageRequires = [ epkgs.ox-hugo epkgs.s ];
          version = "0.1.0";
        })
      ];
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
          ({ pkgs, ...}: {
            home.packages = [ pkgs.nodejs ];
          })
          ({ pkgs, ...}: { home.packages = [ pkgs.gcc ]; })
          ({ pkgs, ...}: {
            home.packages = [
              pkgs.nuspell
              pkgs.hunspellDicts.en_US
            ];
          })
          ({ pkgs, ...}: { home.packages = [ pkgs.ripgrep ]; })
          ./load-path.nix
        ];
        programs.emacs = {
          enable = true;
          package = config.packages.emacs-ross;
        };
      }
    );
  };
}
