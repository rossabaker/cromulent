{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "avatar";
  src = ../../../src/hugo/static/img;
  buildInputs = [ pkgs.imagemagick ];

  buildPhase = ''
    convert profile.jpg -resize 128x128 avatar.jpg
  '';

  installPhase = ''
    mkdir $out
    cp avatar.jpg $out/
  '';
}
