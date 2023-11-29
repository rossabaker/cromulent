{ self, ... }:
{
  flake.nixosModules.netdata =
    { pkgs, lib, ... }:
    lib.mkMerge [
      {
        services.netdata.enable = true;
        services.netdata.config = {
          global = {
            "update every" = 15;
          };
        };
      }
      {
        services.netdata.package = pkgs.netdataCloud;
      }
      {
        nixpkgs.config.allowUnfreePredicate =
          pkg: builtins.elem (lib.getName pkg) [
            "netdata"
          ];
      }
    ];
}
