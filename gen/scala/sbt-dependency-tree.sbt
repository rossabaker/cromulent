libraryDependencies ++= (
  if (VersionNumber(sbtVersion.value).matchesSemVer(SemanticSelector(">=1.8"))) {
    Seq(
      Defaults.sbtPluginExtra(
        ModuleID("org.scala-sbt", "sbt-dependency-tree", sbtVersion.value),
        sbtBinaryVersion.value,
        scalaBinaryVersion.value
      )
    )
  } else Seq.empty
)
