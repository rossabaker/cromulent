let
  domain = "git.rossabaker.com";
in
{
  services.forgejo = {
    enable = true;
    settings = {
      service = {
        DISABLE_REGISTRATION = true;
      };
    };
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:3000/";
    };
  };

  services.nginx.virtualHosts."src.rossabaker.com" = {
    enableACME = true;
    forceSSL = true;
    globalRedirect = domain;
  };
}
