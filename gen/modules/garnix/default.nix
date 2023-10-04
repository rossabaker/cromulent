{ config, lib, ... }:

let
  cfg = config.cromulent.binaryCaches.garnix;
in
{
  imports = [ ../binaryCaches ];

  options = {
    cromulent.binaryCaches.garnix.enable =
      lib.mkEnableOption "Configures Nix to use the Garnix cache as a substituter" // {
        default = true;
      };
  };

  config = lib.mkIf cfg.enable {
    cromulent.binaryCaches.caches = {
      "https://cache.garnix.io/" = "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g=";
    };
  };
}
