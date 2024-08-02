name := "advent-of-code-2015"

scalaVersion := "3.3.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-explain",
  "-explaintypes",
  "-feature",
  "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.16" % Test,
  "org.playframework" %% "play-json" % "3.0.4"
)