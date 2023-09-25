{ inputs, pkgs, lib, ... }:

{
  nixpkgs.overlays = [
    inputs.firefox-darwin.overlay
  ];

  home.packages = [
    pkgs.firefox-bin
    pkgs.rectangle
  ];
}
