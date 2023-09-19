{ inputs, ... }:
{
  flake.darwinModules.base = (
    { config, pkgs, ... }:
    {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages =
        [
          pkgs.cachix
          pkgs.docker
        ];

      # Use a custom configuration.nix location.
      # $ darwin-rebuild switch -I darwin-config=$HOME/.config/nixpkgs/darwin/configuration.nix
      # environment.darwinConfig = "$HOME/.config/nixpkgs/darwin/configuration.nix";

      # Auto upgrade nix package and the daemon service.
      # services.nix-daemon.enable = true;
      nix = {
        settings.substituters = [
          "https://nix-community.cachix.org/"
          "https://rossabaker.cachix.org/"
          "https://typelevel.cachix.org/"
        ];
        settings.trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "rossabaker.cachix.org-1:KK/CQTeAGEurCUBy3nDl9PdR+xX+xtWQ0C/GpNN6kuw="
          "typelevel.cachix.org-1:UnD9fMAIpeWfeil1V/xWUZa2g758ZHk8DvGCd/keAkg="
        ];
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };

      # Create /etc/bashrc that loads the nix-darwin environment.
      programs.zsh.enable = true; # default shell on catalina
      # programs.fish.enable = true;

      services.nix-daemon.enable = true;

      system.keyboard = {
        enableKeyMapping = true;
        remapCapsLockToControl = true;
      };

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 4;

      fonts = {
        fontDir.enable = true;
        fonts = [ pkgs.ibm-plex ];
      };
    }
  );

  flake.darwinModules.default = {
    imports = with inputs.self.darwinModules; [
      base
      garnix
      podman
      postgresql
    ];
  };
}
