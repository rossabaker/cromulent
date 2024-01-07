let
  abe = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOzPacaXi/O4cw/g+Rpjezg3kb7z6OQlIVcaPwlH25ps";
in
{
  "abe-backups-default.age".publicKeys = [ abe ];
  "abe-forgejo-runner-env.age".publicKeys = [ abe ];
}
