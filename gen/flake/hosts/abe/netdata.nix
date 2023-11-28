{ lib, pkgs, ... }:

{
  nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
    "netdata"
  ];

  services.netdata = {
    enable = true;
    package = pkgs.netdataCloud;
    config = {
      global = {
        "update every" = 15;
      };
      ml = {
        "enabled" = "yes";
      };
    };
  };
}
