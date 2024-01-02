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
    streamingProcesses = 7;
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

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
        proxyPass = "http://mastodon-streaming";
        proxyWebsockets = true;
      };
    };
    upstreams.mastodon-streaming = {
      extraConfig = ''
        least_conn;
      '';
      servers = builtins.listToAttrs
        (map (i: {
          name = "unix:/run/mastodon-streaming/streaming-${toString i}.socket";
          value = { };
        }) (lib.range 1 cfg.streamingProcesses));
    };
  };

  users.groups.${cfg.group}.members = [ config.services.nginx.user ];
}
