{ pkgs, ... }:

let
  domain = "avatars.rossabaker.com";
  md5 = "e8711ac481197ac9b13c8f9b3eccffe0";
  sha256 = "fb07324fd106bfe8d217ddd029dd901e1f98fb282844f5ce8191d8ab4a6cb74c";
  avatar = pkgs.callPackage ./avatar.nix {};
  nginxConfig = {
    extraConfig = ''
      root ${avatar};
      try_files /avatar.jpg =404;
    '';
  };
in
{
  services.nginx.virtualHosts.${domain} = {
    forceSSL = true;
    enableACME = true;
    locations."= /avatar/${md5}" = nginxConfig;
    locations."= /avatar/${sha256}" = nginxConfig;
  };
}
