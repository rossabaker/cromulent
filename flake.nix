{
  description = "rossabaker.com";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        defaultPackage = pkgs.stdenv.mkDerivation {
          name = "rossabaker.com";
          src = ./src;
          buildInputs = [ pkgs.emacs ];
          buildPhase = ''
            echo $TMPDIR
            export HOME=$TMPDIR
            ${pkgs.emacs}/bin/emacs -Q --script publish.el $HOME/html
            echo 'rossabaker.com' > $HOME/html/CNAME
          '';
          installPhase = ''
            mkdir $out
            cp -a $HOME/html/* $out
          '';
        };
      }
    );
}
