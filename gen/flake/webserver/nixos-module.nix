{
  services.nginx = {
    enable = true;
    virtualHosts."beta.www.rossabaker.com" = {
      forceSSL = true;
      enableACME = true;
      root = "/var/www/com.rossabaker.www.beta";
    };
  };
}
