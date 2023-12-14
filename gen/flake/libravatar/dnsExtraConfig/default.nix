{ config, ... }:

{
  zones."rossabaker.com" = {
    "_avatars._tcp".srv.data = {
      priority = 0;
      weight = 0;
      port = 80;
      target = "avatars.rossabaker.com";
    };
    "_avatars-sec._tcp".srv.data = {
      priority = 0;
      weight = 0;
      port = 443;
      target = "avatars.rossabaker.com";
    };
  };
}
