
# This is your home-manager configuration file
# Use this to configure your home environment (it replaces ~/.config/nixpkgs/home.nix)

{ inputs, lock, lib, config, pkgs, ... }:

{
  imports = [
    # If you want to use home-manager modules from other flakes (such as nix-colors), use something like:
    # inputs.nix-colors.homeManagerModule

    # Feel free to split up your configuration and import pieces of it here.
    ./modules/emacs
    ./modules/scala
  ];

  # Comment out if you wish to disable unfree packages for your system
  nixpkgs.config.allowUnfree = true;

  # Add stuff for your user as you see fit:
  # programs.neovim.enable = true;
  home.packages = [
    pkgs.gomod2nix
  ];

  programs.home-manager.enable = true;

  programs.git = {
    enable = true;
    ignores = [
      ".bsp/"
      ".direnv/"
      ".metals/"
      "metals.sbt"
    ];
    userName = "Ross A. Baker";
    userEmail = "ross@rossabaker.com";
  };

  programs.gpg = {
    enable = true;
  };

  # Nicely reload system units when changing configs
  systemd.user.startServices = "sd-switch";
}
