{ config, inputs, self, ... }:

let
  dnsConfig = {
    inherit (self) nixosConfigurations;
    extraConfig = { ... }: {
      imports = [
        ../mail/dnsExtraConfig
        ../libravatar/dnsExtraConfig
        ./verification.nix
        ./www.nix
      ];

      defaultTTL = 60;
    };
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
