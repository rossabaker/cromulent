{ config, ... }:

with builtins;
let
  iface = "enp27s0";

  fqdn = "${config.networking.hostName}.${config.networking.domain}";
  ipv4Addresses = config.networking.interfaces.${iface}.ipv4.addresses;
  ipv6Addresses = config.networking.interfaces.${iface}.ipv6.addresses;
in
{
  networking.domains.subDomains.${fqdn} = {
    a.data = map (a: a.address) ipv4Addresses;
    aaaa.data = map (a: a.address) ipv6Addresses;
  };
}
