import AssemblyKeys._ 
assemblySettings

/** Project */
name := "mln"

version := "1.0-SNAPSHOT"

organization := "-"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-optimize")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true

test in assembly := {}

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter {_.data.getName == "minlog-1.2.jar"}
}

/** Dependencies */
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.10.2"  % "compile",
  "org.scalatest" %% "scalatest" % "2.0.M7" % "test",
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)
