{ config, lib, pkgs, ... }:

let
  cfg = config.com.rossabaker.services.libravatar;
in
{
  options.com.rossabaker.services.libravatar = {
    enable = lib.mkEnableOption "Libravatar Nginx host" // {
      default = true;
    };
    domain = lib.mkOption {
      type = lib.types.str;
      example = "avatars.example.com";
    };
    avatarSrc = lib.mkOption {
      type = lib.types.path;
    };
  };

  config = let
    avatars = pkgs.callPackage ./avatar.nix {
      src = cfg.avatarSrc;
    };
  in lib.mkIf cfg.enable {
    services.nginx.virtualHosts."${cfg.domain}" = {
      forceSSL = true;
      enableACME = true;
      locations."~ ^/avatar/([0-9a-f]+)" = {
        extraConfig = ''
          root ${avatars};
          try_files /$1.jpg /$1.png /$1.gif =404;
       '';
      };
    };
  };
}
