{ inputs, pkgs, ... }:

{
  nixpkgs.overlays = [
    inputs.firefox-darwin.overlay
  ];

  home.packages = [ pkgs.firefox-bin ];

  home.file = {
    ".sbt/1.0/work.sbt".source = ./work.sbt;
  };
}
