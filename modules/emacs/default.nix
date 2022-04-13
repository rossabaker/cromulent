{ inputs, pkgs, config, ... }:

let
  emacs = pkgs.emacsGcc;

  withPatches = pkg: patches:
    pkg.overrideAttrs (attrs: { inherit patches; });

  emacs-config = pkgs.stdenv.mkDerivation {
    name = "emacs-config";
    src = ../../src/org/config;
    buildInputs = [
      emacs
    ];
    buildPhase = ''
      ${emacs}/bin/emacs -Q -nw emacs.org --batch -f org-babel-tangle --kill
    '';
    installPhase = ''
      mkdir -p $out/share/emacs/site-lisp
      install *.el* $out/share/emacs/site-lisp
    '';
  };

  compile = name: src: pkgs.stdenv.mkDerivation {
    inherit name src;
    buildInputs = [ emacs ];
    buildPhase = ''
      rm -f ${name}-pkg.el # We don't load 'package
      autoloads=${name}-autoloads.el
      [ -f $autoloads] || ${emacs}/bin/emacs --batch -Q -L . --eval "(make-directory-autoloads \".\" \"$autoloads\")"
      ${emacs}/bin/emacs --batch -Q -L . -f batch-byte-compile *.el
    '';
    installPhase = ''
      mkdir -p $out/share/emacs/site-lisp
      install *.el* $out/share/emacs/site-lisp
    '';
  };

  # https://discourse.nixos.org/t/emacs-exwm-home-manager-and-loading-new-emacs-modules/10097/3
  load-path = pkgs.writeText "load-path.el" ''
    (let ((default-directory (file-name-as-directory
                              "${config.programs.emacs.finalPackage.deps}/share/emacs/site-lisp/"))
          (normal-top-level-add-subdirs-inode-list nil))
    (normal-top-level-add-subdirs-to-load-path))
  '';
in
{
  home.packages = [
    pkgs.fd
    pkgs.mdl
    pkgs.metals
    pkgs.pythonPackages.pyhocon
    pkgs.ripgrep
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = builtins.readFile "${emacs-config}/share/emacs/site-lisp/default.el";
      package = emacs;
      override = epkgs: epkgs // {
        benchmark-init = withPatches epkgs.benchmark-init [
          ./patches/benchmark-init/pr00016.diff
          ./patches/benchmark-init/pr00017.diff
        ];
        fill-sentences-correctly = compile "fill-sentences-correctly" inputs.fill-sentences-correctly;
        hocon-mode = compile "hocon-mode" inputs.hocon-mode;
        scala-mode = compile "scala-mode" inputs.scala-mode;
        unmodified-buffer = compile "unmodified-buffer" inputs.unmodified-buffer;
      };
    };
    extraPackages = epkgs: [
      epkgs.use-package
      emacs-config
    ];
  };

  xdg.configFile = {
    "emacs/early-init.el".source = ./early-init.el;
    "emacs/load-path.el".source = load-path;
  };
}
