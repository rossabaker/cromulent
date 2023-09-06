{ moduleWithSystem, ... }: {
  flake.homeManagerModules.scala = moduleWithSystem(
    perSystem@{ pkgs }: {
      home.file = {
              ".sbt/1.0/sonatype.sbt".source = ./sonatype.sbt;
        ".sbt/1.0/plugins/sbt-dependency-submission.sbt".source = ./sbt-dependency-submission.sbt;
              ".sbt/1.0/plugins/sbt-rewarn.sbt".source = ./sbt-rewarn.sbt;
              ".sbt/1.0/plugins/sbt-updates.sbt".source = ./sbt-updates.sbt;
              ".sbt/1.0/plugins/dependency-tree.sbt".source = ./dependency-tree.sbt;
      };

      home.packages = [
              (pkgs.sbt.override { jre = pkgs.jdk17; })
              (pkgs.scala-cli.override { jre = pkgs.jdk17; })
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
  );
}
