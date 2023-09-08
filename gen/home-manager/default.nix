{ moduleWithSystem, ... }: {
  flake.homeManagerModules.base = moduleWithSystem(
    perSystem@{ pkgs }: {
      # Comment out if you wish to disable unfree packages for your system
      nixpkgs.config.allowUnfree = true;

      # Add stuff for your user as you see fit:
      # programs.neovim.enable = true;
      home.packages = [
        pkgs.coreutils
        pkgs.element-desktop
        pkgs.jq
        pkgs.nixpkgs-fmt
        pkgs.postgresql_14
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
          ".DS_Store"
          ".direnv/"
        ];
        userName = "Ross A. Baker";
        userEmail = "ross@rossabaker.com";
      };

      programs.gpg = {
        enable = true;
      };

      programs.zsh.enable = true;

      # Nicely reload system units when changing cnofigs
      systemd.user.startServices = "sd-switch";
    }
  );
}
