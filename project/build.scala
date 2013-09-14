import sbt._
import Keys._

object PrettyPrinterBuild extends Build {
   val prettyPrinter = Project("scala-pretty-printer", file(".")) settings (
    organization := "org.scala-lang",
    name := "scala-pretty-printer",
    version := "0.2.0",
    scalaVersion := "2.10.2",
    //scalaBinaryVersion <<= scalaVersion,
    //crossVersion := CrossVersion.full,
    //exportJars := true,
    libraryDependencies <++= scalaVersion apply dependencies
  )

  def dependencies(sv: String) = Seq(
    "org.scala-lang" % "scala-compiler" % sv) 
}
