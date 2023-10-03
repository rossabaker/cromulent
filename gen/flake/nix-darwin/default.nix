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

      # Auto upgrade nix package and the daemon service.
      # services.nix-daemon.enable = true;
      nix = {
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };

      # Create /etc/bashrc that loads the nix-darwin environment.
      programs.zsh.enable = true; # default shell on catalina

      services.nix-daemon.enable = true;

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
      cachix
      keyboard
      podman
      postgresql
    ];
  };
}
