{ config, ... }:

let
  vhosts = builtins.attrNames config.services.nginx.virtualHosts;
  fqdn = "${config.networking.hostName}.${config.networking.domain}";
in
{
  networking.domains.subDomains =
    builtins.listToAttrs (builtins.map (vhost: {
      name = vhost;
      value = { cname.data = "${fqdn}."; };
    }) vhosts);
}
