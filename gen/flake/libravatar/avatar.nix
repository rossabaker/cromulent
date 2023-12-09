{ pkgs, lib, src, ... }:

pkgs.stdenv.mkDerivation {
  inherit src;
  name = "libravatar-avatars";
  buildInputs = [ pkgs.imagemagick ];
  buildPhase = builtins.readFile ./prepare-images;
}
