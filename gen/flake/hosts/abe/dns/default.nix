{ config, ... }:

{
  imports = [
   ./addresses.nix
   ./cnames.nix
  ];

  networking.domains = {
    enable = true;
    baseDomains."rossabaker.com" = {};
  };
}
