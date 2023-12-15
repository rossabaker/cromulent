resource "hetznerdns_zone" "cromulent" {
  name = "rossabaker.com"
  ttl  = 8 * 60 * 60 # seconds
}
