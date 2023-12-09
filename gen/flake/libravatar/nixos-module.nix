{ pkgs, ... }:

let
  domain = "avatars.rossabaker.com";
  avatar = pkgs.callPackage ./avatar.nix {
    src = ../../../src/avatars;
  };
in
{
  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    locations."~ ^/avatar/([0-9a-f]+)" = {
      extraConfig = ''
        root ${avatar};
        try_files /$1.jpg /$1.png /$1.gif =404;
      '';
    };
  };
}
