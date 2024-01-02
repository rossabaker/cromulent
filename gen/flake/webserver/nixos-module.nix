{ config, ... }:

let
  domain = "beta.www.rossabaker.com";
in
{
  services.nginx = {
    enable = true;
    virtualHosts.${domain} = {
      forceSSL = true;
      enableACME = true;
      root = "/var/www/com.rossabaker.www.beta";
    };
  };
}
