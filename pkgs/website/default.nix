{ emacs, stdenv }:

let
  siteEmacs = emacs.pkgs.withPackages (epkgs: [
    epkgs.esxml
  ]);
in
  stdenv.mkDerivation {
    name = "rossabaker.com";
    src = ./src;
    buildInputs = [ siteEmacs ];
    buildPhase = ''
      echo $TMPDIR
      export HOME=$TMPDIR
      ${siteEmacs}/bin/emacs -Q --script publish.el $HOME/html
      echo 'rossabaker.com' > $HOME/html/CNAME
    '';
    installPhase = ''
      mkdir $out
      cp -a $HOME/html/* $out
    '';
  }
