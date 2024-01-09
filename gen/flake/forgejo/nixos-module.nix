{ config, ... }:

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
      server = {
        ROOT_URL = "https://${domain}/";
        LANDING_PAGE = "explore";
      };
    };
  };

  services.nginx = {
    appendHttpConfig = ''
      map $uri $forgejo_access_log {
        default 1;
        /api/actions/runner.v1.RunnerService/FetchTask 0;
      }
    '';
  };

  services.nginx.virtualHosts.${domain} = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://localhost:3000/";
      extraConfig = ''
        access_log /var/log/nginx/access.log combined if=$forgejo_access_log;
      '';
    };
  };
}
