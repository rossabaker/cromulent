resource "hetznerdns_record" "com_rossabaker_hetzner_abe_a" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "A"
  name     = "abe.hetzner"
  value    = "95.216.32.37"
}

resource "hetznerdns_record" "com_rossabaker_hetzner_abe_aaaa" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "AAAA"
  name     = "abe.hetzner"
  value    = "2a01:4f9:2a:2047::1"
}

resource "hetznerdns_record" "com_rossabaker_git" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "CNAME"
  name     = "git"
  value    = "abe.hetzner.rossabaker.com."
}

resource "hetznerdns_record" "com_rossabaker_matrix" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "CNAME"
  name     = "matrix"
  value    = "abe.hetzner.rossabaker.com."
}

resource "hetznerdns_record" "com_rossabaker_social" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "CNAME"
  name     = "social"
  value    = "abe.hetzner.rossabaker.com."
}

resource "hetznerdns_record" "com_rossabaker_www_beta" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "CNAME"
  name     = "beta.www"
  value    = "abe.hetzner.rossabaker.com."
}

resource "hetznerdns_record" "com_rossabaker_avatars" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "CNAME"
  name     = "avatars"
  value    = "abe.hetzner.rossabaker.com."
  ttl      = 60
}

resource "hetznerdns_zone" "com_rossabaker" {
  name = "rossabaker.com"
  ttl  = 8 * 60 * 60 # seconds
}

resource "hetznerdns_record" "com_rossabaker_openpgp4fpr" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "TXT"
  name     = "@"
  value    = "openpgp4fpr:904c153733dbb0106915c0bd975be5bc29d92ca5"
}

resource "hetznerdns_record" "com_rossabaker_apex" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "A"
  name     = "@"
  value    = "75.2.60.5"
}

resource "hetznerdns_record" "com_rossabaker_www" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "CNAME"
  name     = "www"
  value    = "rossabaker.com."
}

locals {
  poboxes = range(1, 6)
}

resource "hetznerdns_record" "com_rossabaker_mx_pobox" {
  count   = length(local.poboxes)
  zone_id = hetznerdns_zone.com_rossabaker.id
  type    = "MX"
  name    = "@"
  value   = "10 mx-${count.index + 1}.pobox.com."
}

resource "hetznerdns_record" "com_rossabaker_spf" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "TXT"
  name     = "@"
  value    = jsonencode("v=spf1 include:pobox.com -all")
}

resource "hetznerdns_record" "com_rossabaker_dkim" {
  zone_id = hetznerdns_zone.com_rossabaker.id
  type     = "TXT"
  name     = "2020-06.pbsmtp._domainkey"
  value    = jsonencode("v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDIfBzlZiuOljOEgYr0nrIieWvmn9x4mrXlIqgw64oasNb/wn62Yai4APbQ4rAdwwEj2vI0FVs2Y5oTUKmPq+RSsWJKmdEWjv9zUKK+GNjVJ0mVBX75vU1nEwWUeS+Wz4haQxMVXQRrbCovQNoQjFcSX9ERdAbZVzXsf/0kDNzdiQIDAQAB")
}

resource "hetznerdns_record" "com_rossabaker_avatars_srv" {
  zone_id  = hetznerdns_zone.com_rossabaker.id
  type     = "SRV"
  name     = "_avatars._tcp"
  value    = "0 0 80 avatars.rossabaker.com."
}

resource "hetznerdns_record" "com_rossabaker_avatars_srv_sec" {
  zone_id  = hetznerdns_zone.com_rossabaker.id
  type     = "SRV"
  name     = "_avatars-sec._tcp"
  value    = "0 0 443 avatars.rossabaker.com."
}
