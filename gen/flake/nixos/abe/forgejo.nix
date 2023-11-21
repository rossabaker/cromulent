{
  services.forgejo = {
    enable = true;
    settings = {
      service = {
        DISABLE_REGISTRATION = true;
      };
    };
  };

  services.nginx.virtualHosts."src.rossabaker.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:3000/";
    };
  };
}
