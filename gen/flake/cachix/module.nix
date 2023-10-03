let
  module =
    { config, lib, ... }:
    let
      cfg = config.cromulent.cache.cachix;
    in
      {
        options.cromulent.cache.cachix = {
          enabled =
            lib.mkEnableOption "Configures Nix to use various Cachix instances as a substituter" // {
              default = true;
            };

          cachices =
            lib.mkOption {
              type = lib.types.attrsOf lib.types.string;
              default = {
                "https://nix-community.cachix.org/" = "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs=";
                "https://rossabaker.cachix.org/" = "rossabaker.cachix.org-1:KK/CQTeAGEurCUBy3nDl9PdR+xX+xtWQ0C/GpNN6kuw=";
                "https://typelevel.cachix.org/" = "typelevel.cachix.org-1:UnD9fMAIpeWfeil1V/xWUZa2g758ZHk8DvGCd/keAkg=";
              };
            };
        };

        config.flake.darwinModules.cachix =
          lib.mkIf cfg.enabled {
            nix.settings = {
              substituters = builtins.attrNames cfg.cachices;
              trusted-public-keys = builtins.attrValues cfg.cachices;
            };
          };
      };
in
{
  flake = {
    nixosModules.cachix = module;
    darwinModules.cachix = module;
    homeModules.cachix = module;
  };
}
