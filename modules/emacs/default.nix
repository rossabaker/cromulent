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

  hocon-mode = compile "hocon-mode" inputs.hocon-mode;
in
{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      config = ./init.el;
      package = emacs;
    };
    extraPackages = epkgs: [
      epkgs.use-package
      hocon-mode
    ];
  };

  xdg.configFile."emacs/init.el".source = ./init.el;
}
