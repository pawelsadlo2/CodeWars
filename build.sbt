name := "FindTheSmallest"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-reflect" % "latest.integration",
  //test
  "org.scalactic" %% "scalactic" % "latest.integration",
  "org.scalatest" %% "scalatest" % "latest.integration" % "test"
)

scalacOptions := Seq("-unchecked", "-deprecation")