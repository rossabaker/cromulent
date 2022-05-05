{ pkgs, src }:

import (pkgs.stdenv.mkDerivation {
  name = "tangle-${builtins.baseNameOf src}";
  inherit src;
  buildInputs = [
    pkgs.emacs
  ];
  buildPhase = ''
    ${pkgs.emacs}/bin/emacs -Q -nw index.org --batch -f org-babel-tangle --kill
  '';
  installPhase = ''
    mkdir $out
    cp -r ./. $out
  '';
})
