{ pkgs, ... }:

{
  home.file = {
    ".sbt/1.0/sonatype.sbt".source = ./sonatype.sbt;
    ".sbt/1.0/plugins/sbt-rewarn.sbt".source = ./sbt-rewarn.sbt;
    ".sbt/1.0/plugins/sbt-updates.sbt".source = ./sbt-updates.sbt;
    ".sbt/1.0/plugins/dependency-tree.sbt".source = ./dependency-tree.sbt;
  };

  home.packages = [
    pkgs.sbt
  ];

  programs.git = {
    ignores = [
      ".bloop/"
      ".bsp/"
      ".metals/"
      "metals.sbt"
    ];
  };
}
