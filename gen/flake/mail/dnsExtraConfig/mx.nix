{ lib, ... }:

{
  zones."rossabaker.com"."".mx.data =
    map (i: {
      preference = 10;
      exchange = "mx-${toString i}.pobox.com";
    }) (lib.lists.range 1 5);
}
