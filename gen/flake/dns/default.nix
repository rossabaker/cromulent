{ config, inputs, self, ... }:

let
  dnsConfig = {
    inherit (self) nixosConfigurations;
    extraConfig = import ../mail/dnsExtraConfig;
  };
in
{
  perSystem = { pkgs, ... }:
    let
      generate = inputs.nixos-dns.utils.generate pkgs;
    in
    {
      packages.zoneFiles = generate.zoneFiles dnsConfig;
    };
}
