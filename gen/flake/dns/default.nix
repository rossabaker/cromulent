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
        packages = {
          zoneFiles = generate.zoneFiles dnsConfig;
          octodns = generate.octodnsConfig {
            inherit dnsConfig;
            config.providers = {
              hetzner = {
                class = "octodns_hetzner.HetznerProvider";
                token = "env/HETZNER_DNS_API_TOKEN";
              };
              config = {
                check_origin = false;
              };
            };
            zones = {
              "rossabaker.com." = inputs.nixos-dns.utils.octodns.generateZoneAttrs [ "hetzner" ];
            };
          };
        };
      };
}
