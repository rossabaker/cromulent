{ inputs, pkgs, ... }:

let
  emacs = pkgs.emacsGcc;

  withPatches = pkg: patches:
    pkg.overrideAttrs(attrs: { inherit patches; });

  compile = name: src: pkgs.stdenv.mkDerivation {
    inherit name src;
    buildInputs = [ emacs ];
    buildPhase = ''
      ${emacs}/bin/emacs -Q -nw -L . --batch -f batch-byte-compile *.el
    '';
    installPhase = ''
      mkdir -p $out/share/emacs/site-lisp
      install *.el* $out/share/emacs/site-lisp
    '';
  };
in
{
  home.packages = [
    pkgs.pythonPackages.pyhocon
  ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = emacs;
      override = epkgs: epkgs // {
        benchmark-init = withPatches epkgs.benchmark-init [
          ./patches/benchmark-init/pr00016.diff
        ];
        hocon-mode = compile "hocon-mode" inputs.hocon-mode;
        scala-mode = compile "scala-mode" inputs.scala-mode;
      };
    };
    extraPackages = epkgs: [
      epkgs.use-package
    ];
  };

  xdg.configFile = {
    "emacs/early-init.el".source = ./early-init.el;
    "emacs/init.el".source = ./init.el;
  };
}
