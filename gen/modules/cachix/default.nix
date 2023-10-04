{ config, lib, ... }:
let
  cfg = config.cromulent.cache.cachix;
in
  {
    imports = [ ../binaryCaches ];

    options.cromulent.cache.cachix = {
      rossabaker.enabled =
        lib.mkEnableOption "Configures Nix to use `rossabaker.cachix.org` as a binary cache." // {
          default = true;
        };
      typelevel.enabled =
        lib.mkEnableOption "Configures Nix to use `typelevel.cachix.org` as a binary cache." // {
          default = true;
        };
      nix-community.enabled =
        lib.mkEnableOption "Configures Nix to use `nix-community.cachix.org` as a binary cache." // {
          default = true;
        };
    };

    config = lib.mkMerge [
      (lib.mkIf cfg.rossabaker.enabled {
        cromulent.binaryCaches.caches = {
          "https://rossabaker.cachix.org/" = "rossabaker.cachix.org-1:KK/CQTeAGEurCUBy3nDl9PdR+xX+xtWQ0C/GpNN6kuw=";
        };
      })
      (lib.mkIf cfg.typelevel.enabled {
        cromulent.binaryCaches.caches = {
          "https://typelevel.cachix.org/" = "typelevel.cachix.org-1:UnD9fMAIpeWfeil1V/xWUZa2g758ZHk8DvGCd/keAkg=";
        };
      })
      (lib.mkIf cfg.nix-community.enabled {
        cromulent.binaryCaches.caches = {
          "https://nix-community.cachix.org/" = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
        };
      })
    ];
  }
