{ config, ... }:

{
  imports = [
   ./addresses.nix
  ];

  networking.domains = {
    enable = true;
    baseDomains."rossabaker.com" = {};
  };
}
