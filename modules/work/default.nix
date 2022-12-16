{ inputs, pkgs, lib, ... }:

{
  nixpkgs.overlays = [
    inputs.firefox-darwin.overlay
  ];

  home.packages = [
    pkgs.firefox-bin
    pkgs.rectangle
    pkgs.slack
    pkgs.zoom-us
  ];

  home.file = {
    ".sbt/1.0/work.sbt".source = ./work.sbt;
  };

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "slack"
    "zoom"
  ];
}
