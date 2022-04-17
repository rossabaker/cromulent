{ emacsGcc, srcOnly, stdenv }:

let
  siteEmacs = emacsGcc.pkgs.withPackages (epkgs: [
    epkgs.esxml
    epkgs.htmlize
  ]);
in
  stdenv.mkDerivation rec {
    name = "rossabaker.com";
    srcs = ../../src;
    buildInputs = [ siteEmacs ];
    buildPhase = ''
      export HOME=$TMPDIR
      ${siteEmacs}/bin/emacs -Q --script publish.el $HOME/html
      echo 'rossabaker.com' > $HOME/html/CNAME
    '';
    installPhase = ''
      mkdir $out
      cp -a $HOME/html/* $out
    '';
  }
