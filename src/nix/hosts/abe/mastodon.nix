{ config, lib, ... }:

let
  cfg = config.services.mastodon;
in
{
  services.mastodon = {
    enable = true;
    localDomain = "rossabaker.com";
    extraConfig = {
      WEB_DOMAIN = "social.rossabaker.com";
    };
    configureNginx = false;
    smtp.fromAddress = "";
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  security.acme = {
    acceptTerms = true;
    defaults.email = "ross@rossabaker.com";
  };

  # The default module configures localDomain.  We need to configure
  # the WEB_DOMAIN.  This is a nasty copy-and-paste.
  services.nginx = {
    enable = true;
    recommendedProxySettings = true; # required for redirections to work
    virtualHosts."${cfg.extraConfig.WEB_DOMAIN}" = {
      root = "${cfg.package}/public/";
      # mastodon only supports https, but you can override this if you offload tls elsewhere.
      forceSSL = lib.mkDefault true;
      enableACME = lib.mkDefault true;

      locations."/system/".alias = "/var/lib/mastodon/public-system/";

      locations."/" = {
        tryFiles = "$uri @proxy";
      };

      locations."@proxy" = {
        proxyPass = (if cfg.enableUnixSocket then "http://unix:/run/mastodon-web/web.socket" else "http://127.0.0.1:${toString(cfg.webPort)}");
        proxyWebsockets = true;
      };

      locations."/api/v1/streaming/" = {
        proxyPass = (if cfg.enableUnixSocket then "http://unix:/run/mastodon-streaming/streaming.socket" else "http://127.0.0.1:${toString(cfg.streamingPort)}/");
        proxyWebsockets = true;
      };

      extraConfig = ''
        client_max_body_size 30m;
      '';
    };
  };

  users.groups.${cfg.group}.members = [ config.services.nginx.user ];
}
