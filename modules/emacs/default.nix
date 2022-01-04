{ inputs, pkgs, ... }:

let
  emacs = pkgs.emacsGcc;

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
        hocon-mode = compile "hocon-mode" inputs.hocon-mode;
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
