{ moduleWithSystem, ... }: {
  flake.homeModules.base = moduleWithSystem(
    perSystem@{ pkgs, inputs' }: {
      # Curiously not a default of home-manager, but required once we
      # start configuring Nix otherwise.
      nix.package = pkgs.nix;

      # Comment out if you wish to disable unfree packages for your system
      nixpkgs.config.allowUnfree = true;

      # Add stuff for your user as you see fit:
      # programs.neovim.enable = true;
      home.packages = [
        pkgs.coreutils
        pkgs.element-desktop
        pkgs.gh
        pkgs.jq
        pkgs.nixpkgs-fmt
        pkgs.postgresql_14
        inputs'.agenix.packages.agenix
      ];

      programs.direnv = {
        enable = true;
        enableBashIntegration = true;
        enableZshIntegration = true;
      };

      programs.home-manager.enable = true;

      programs.git = {
        enable = true;
        ignores = [
          ".direnv/"
        ];
        userName = "Ross A. Baker";
        userEmail = "ross@rossabaker.com";
      };

      programs.gpg = {
        enable = true;
      };

      programs.zsh.enable = true;

      # Nicely reload system units when changing configs
      systemd.user.startServices = "sd-switch";
    }
  );
}
