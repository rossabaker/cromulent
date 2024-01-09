{ config, ... }:

let
  domain = "beta.www.rossabaker.com";
  redirectFile = "${config.users.users.www.home}/public/_redirects.map";
in
{
  services.nginx = {
    enable = true;
    appendHttpConfig = ''
      map $uri $redirectedUri {
        default "";
        include ${redirectFile};
      }
    '';
    mapHashBucketSize = 128;

    virtualHosts.${domain} = {
      forceSSL = true;
      enableACME = true;
      root = "${config.users.users.www.home}/public";
      locations."/".extraConfig = ''
        if ($redirectedUri) {
          return 301 $redirectedUri;
        }
      '';
      locations."= /.well-known/webfinger" = {
        return = "302 https://social.rossabaker.com/.well-known/webfinger$is_args$args";
      };
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
