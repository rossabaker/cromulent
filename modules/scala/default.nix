{ pkgs, ... }:

{
  home.file.".sbt/1.0/sonatype.sbt".source = ./sonatype.sbt;

  home.packages = [
    pkgs.sbt
  ];
}
