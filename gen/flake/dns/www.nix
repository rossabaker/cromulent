{ ... }:
{
  zones."rossabaker.com" = {
    "".a.data = "75.2.60.5";
    "".a.ttl = 30;
    "www".cname.data = "rossabaker.com.";
  };
}
