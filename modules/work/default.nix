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

  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "slack"
    "zoom"
  ];
}
