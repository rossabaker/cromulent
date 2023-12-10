{ config, ... }:

{
  com.rossabaker.services.libravatar = {
    domain = "avatars.rossabaker.com";
    avatarSrc = ../../../../src/avatars;
  };
}
