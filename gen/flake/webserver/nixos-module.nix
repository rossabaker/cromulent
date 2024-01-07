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
      root = "${config.users.users.www.home}/public";
    };
  };

  users.users.www = {
    description = "Owns the web root for www";
    isSystemUser = true;
    home = "/var/lib/www";
    createHome = true;
    homeMode = "755";
    group = "www";
    useDefaultShell = true;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAID+UMMyU0dpJMX2hVRAu2tTqvcTIueNLdJmG049iSrtu"
    ];
  };

  users.groups.www = { };
}
