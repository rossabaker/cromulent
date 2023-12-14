{ config, inputs, self, ... }:

let
  dnsConfig = {
    inherit (self) nixosConfigurations;
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
