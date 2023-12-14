{ config, ... }:

with builtins;
let
  iface = config.networking.interfaces."enp27s0";
  domain = config.networking.domain;
  fqdn = "${config.networking.hostName}.${domain}";
in
{
  imports = [ ./addresses.nix ];

  networking.domains = {
    enable = true;
    baseDomains."${domain}" = {};
  };
}
