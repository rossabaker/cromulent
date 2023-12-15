resource "hetznerdns_zone" "cromulent" {
  name = "rossabaker.com"
  ttl  = 8 * 60 * 60 # seconds
}

locals {
  poboxes = range(1, 6)
}

resource "hetznerdns_record" "com_rossabaker_mx_pobox" {
  count   = length(local.poboxes)
  zone_id = hetznerdns_zone.cromulent.id
  type    = "MX"
  name    = "@"
  value   = "10 mx-${count.index + 1}.pobox.com."
}

resource "hetznerdns_record" "com_rossabaker_spf" {
  zone_id = hetznerdns_zone.cromulent.id
  type     = "TXT"
  name     = "@"
  value    = jsonencode("v=spf1 include:pobox.com -all")
}

resource "hetznerdns_record" "com_rossabaker_dkim" {
  zone_id = hetznerdns_zone.cromulent.id
  type     = "TXT"
  name     = "2020-06.pbsmtp._domainkey"
  value    = jsonencode("v=DKIM1; k=rsa; p=MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQDIfBzlZiuOljOEgYr0nrIieWvmn9x4mrXlIqgw64oasNb/wn62Yai4APbQ4rAdwwEj2vI0FVs2Y5oTUKmPq+RSsWJKmdEWjv9zUKK+GNjVJ0mVBX75vU1nEwWUeS+Wz4haQxMVXQRrbCovQNoQjFcSX9ERdAbZVzXsf/0kDNzdiQIDAQAB")
}
