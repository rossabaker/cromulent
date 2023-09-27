{ config, lib, moduleWithSystem, ... }: {
  flake.homeModules.darwin = moduleWithSystem(
    perSystem@{ pkgs }: lib.mkIf pkgs.stdenv.isDarwin {
      nixpkgs.overlays = [
        config.flake.inputs.firefox-darwin.overlay
      ];

      home.packages = [
        pkgs.firefox-bin
        pkgs.rectangle
      ];

      programs.git = {
        ignores = [
          ".DS_Store"
        ];
      };
    }
  );
}
