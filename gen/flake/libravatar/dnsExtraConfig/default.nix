{ config, ... }:

let
  priority = 0;
  weight = 0;
  target = "avatars.rossabaker.com";
in
{
  zones."rossabaker.com" = {
    "_avatars._tcp".srv.data = {
      inherit priority weight target;
      port = 80;
    };
    "_avatars-sec._tcp".srv.data = {
      inherit priority weight target;
      port = 443;
    };
  };
}
