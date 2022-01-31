{ pkgs, ... }:

{
  home.file = {
    ".sbt/1.0/work.sbt".source = ./work.sbt;
  };
}
