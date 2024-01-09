{ config, ... }:

with builtins;
let
  iface = "enp27s0";

  fqdn = "${config.networking.hostName}.${config.networking.domain}";
  ipv4Addresses = config.networking.interfaces.${iface}.ipv4.addresses;
  ipv6Addresses = config.networking.interfaces.${iface}.ipv6.addresses;

  records = {
    a.data = map (a: a.address) ipv4Addresses;
    a.ttl = 30;
    aaaa.data = map (a: a.address) ipv6Addresses;
    aaaa.ttl = 30;
  };
in
{
  networking.domains.subDomains."${fqdn}" = records;
  networking.domains.subDomains."rossabaker.com" = records;
}
