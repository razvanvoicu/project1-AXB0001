val scala3Version = "3.1.0"
val globalVersion = "0.1.0-SNAPSHOT"

val deps = Seq(
  "org.scalactic" %% "scalactic" % "3.2.10",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test"
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "project1",
    version := globalVersion,
    scalaVersion := scala3Version,
    libraryDependencies ++= deps
  )

// finalized
