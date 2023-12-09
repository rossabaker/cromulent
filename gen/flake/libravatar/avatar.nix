{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation {
  name = "avatar";
  src = lib.cleanSourceWith {
    src = ../../../src/hugo/static/img;
    filter = (path: type: (baseNameOf path) == "profile.jpg");
  };
  buildInputs = [ pkgs.imagemagick ];

  buildPhase = ''
    convert profile.jpg -resize 128x128 avatar.jpg
  '';

  installPhase = ''
    mkdir $out
    cp avatar.jpg $out/
  '';
}
