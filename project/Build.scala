import sbt._
import Keys._

object MlnBuild extends Build {
  lazy val scCore = ProjectRef(file("../signal-collect"), id = "signal-collect")
  val scMln = Project(id = "mln",
    base = file(".")) dependsOn (scCore)
}
