{ config, ... }:

{
  com.rossabaker.services.libravatar = {
    domain = "avatars.rossabaker.com";
    avatars = ../../../../src/avatars;
  };
}
