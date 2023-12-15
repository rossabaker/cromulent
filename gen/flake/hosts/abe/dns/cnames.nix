{ config, lib, ... }:

let
  cname = "${config.networking.hostName}.${config.networking.domain}.";
in
{
  networking.domains.subDomains =
    lib.listToAttrs (map (host: { name = host; value = { cname.data = cname; };}) [
      "social.rossabaker.com"
      "beta.www.rossabaker.com"
    ]);
}
