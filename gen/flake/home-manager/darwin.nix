{ inputs, lib, moduleWithSystem, ... }: {
  flake.homeModules.darwin = moduleWithSystem (
    perSystem@{ pkgs }:
    # nixpkgs.overlays doesn't work here
    let pkgs' = pkgs.extend inputs.firefox-darwin.overlay;
    in lib.mkIf pkgs.stdenv.isDarwin {
      home.packages = [
        pkgs'.firefox-bin
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
