// For obvious reasons, these files are not checked in.
credentials ++= Seq(
  Credentials(Path.userHome / ".sbt" / "banno-credentials.properties"),
  Credentials(Path.userHome / ".sbt" / "geezeo-credentials.properties"),
)
