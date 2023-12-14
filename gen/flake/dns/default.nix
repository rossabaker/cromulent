{ config, inputs, self, ... }:

let
  dnsConfig = {
    inherit (self) nixosConfigurations;
    extraConfig = { ... }: {
      imports = [
        ../mail/dnsExtraConfig
        ../libravatar/dnsExtraConfig
        ./verification.nix
      ];
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
