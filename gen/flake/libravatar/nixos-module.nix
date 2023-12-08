let
  profile = ../../../src/img;
in
{
  services.nginx.virtualHosts."avatars.rossabaker.com" = {
    forceSSL = true;
    enableACME = true;
    locations."= /avatar/e8711ac481197ac9b13c8f9b3eccffe0" = {
      root = profile;
      extraConfig = ''
        try_files /avatar.jpg =404;
      '';
    };
    locations."= /avatar/fb07324fd106bfe8d217ddd029dd901e1f98fb282844f5ce8191d8ab4a6cb74c" = {
      root = profile;
      extraConfig = ''
        try_files /avatar.jpg =404;
      '';
    };
  };
}
