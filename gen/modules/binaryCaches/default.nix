{ config, lib, ... }:
let
  cfg = config.cromulent.binaryCaches;
in
  {
    options.cromulent.binaryCaches = {
      enabled =
        lib.mkEnableOption "Configure Nix to use various binary caches" // {
          default = true;
        };

      caches =
        lib.mkOption {
          type = lib.types.attrsOf lib.types.str;
          default = {};
        };
    };

    config =
      lib.mkIf cfg.enabled {
        nix.settings = {
          substituters = builtins.attrNames cfg.caches;
          trusted-public-keys = builtins.attrValues cfg.caches;
        };
      };
  }
