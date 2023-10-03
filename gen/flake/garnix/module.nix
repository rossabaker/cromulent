{ config, lib, ... }:

let
  cfg = config.cromulent.cache.garnix;
in
{
  options = {
    cromulent.cache.garnix.enable =
      lib.mkEnableOption "Configures Nix to use the Garnix cache as a substituter" // {
        default = true;
      };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      nix = {
        settings.substituters = [
          "https://cache.garnix.io/"
        ];
        settings.trusted-public-keys = [
          "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
        ];
      };
    })
  ];
}
