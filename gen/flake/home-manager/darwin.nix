{ inputs, lib, moduleWithSystem, ... }: {
  flake.homeModules.darwin = moduleWithSystem (
    perSystem@{ pkgs }:
    let pkgs' = pkgs.extend inputs.firefox-darwin.overlay;
    in {
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
