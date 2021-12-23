{ pkgs, ... }:

{
  home.file.".sbt/1.0/banno.sbt".source = ./banno.sbt;
}
