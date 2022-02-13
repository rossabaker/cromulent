{ emacs, stdenv }:

stdenv.mkDerivation {
  name = "rossabaker.com";
  src = ./src;
  buildInputs = [ emacs ];
  buildPhase = ''
    echo $TMPDIR
    export HOME=$TMPDIR
    ${emacs}/bin/emacs -Q --script publish.el $HOME/html
    echo 'rossabaker.com' > $HOME/html/CNAME
  '';
  installPhase = ''
    mkdir $out
    cp -a $HOME/html/* $out
  '';
}
