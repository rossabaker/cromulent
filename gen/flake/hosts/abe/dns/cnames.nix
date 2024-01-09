{ config, ... }:

let
  vhosts = builtins.attrNames config.services.nginx.virtualHosts;
  baseDomains = builtins.attrNames config.networking.domains.baseDomains;
  isBaseDomain = x: builtins.elem x baseDomains;
  fqdn = "${config.networking.hostName}.${config.networking.domain}";
in
{
  networking.domains.subDomains =
    builtins.listToAttrs (builtins.map (vhost: {
      name = vhost;
      value = { cname.data = "${fqdn}."; };
    }) (builtins.filter (x: !isBaseDomain x) vhosts));
}
