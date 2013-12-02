import sbt._
import Keys._
import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.site.JekyllSupport.Jekyll
import com.typesafe.sbt.SbtGhPages.ghpages
import com.typesafe.sbt.SbtGit.git

object SprinterBuild extends Build {
   lazy val defaults = Defaults.defaultSettings ++ Seq(
    organization := "org.scala-lang",
    name := "sprinter",
    version := "0.2.0",
    scalaVersion := "2.10.3",
    //scalaBinaryVersion <<= scalaVersion,
    //crossVersion := CrossVersion.full,
    //exportJars := true,


    publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
  ) 

  lazy val compilerSettings = Seq(
    libraryDependencies <++= scalaVersion apply compilerDeps
  )

  lazy val reflectionSettings = Seq(
    libraryDependencies <++= scalaVersion apply reflectDeps
  )

  lazy val testSettings = Seq(
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-q", "-v", "-s", "-a"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    // quasi quote support // TODO: move this to tests only
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "junit" % "junit-dep" % "4.10" % "test",
      "com.novocode" % "junit-interface" % "0.10-M4" % "test"
    )
  )

  lazy val websiteSettings: Seq[Setting[_]] = (
    site.settings ++
    ghpages.settings ++
    site.includeScaladoc() ++
    site.jekyllSupport() ++
    Seq(
      git.remoteRepo := "https://github.com/VladimirNik/sprinter.git",
      includeFilter in Jekyll := ("*.html" | "*.png" | "*.js" | "*.css" | "CNAME")
    )
  )

  def compilerDeps(sv: String) = Seq(
    "org.scala-lang" % "scala-compiler" % sv
  ) 

  def reflectDeps(sv: String) = Seq(
    "org.scala-lang" % "scala-reflect" % sv
  ) 

  lazy val _sprinter       = Project(id = "root",               base = file("."), settings = Project.defaultSettings ++ defaults ++ websiteSettings ++ Seq(publishArtifact := false)) aggregate (sprinter_tree, sprinter_type, sprinter_test)
  lazy val sprinter_tree   = Project(id = "sprinter-tree",      base = file("components/treePrinter"), settings = defaults ++ reflectionSettings ++ Seq(name := "sprinter-tree"))
  lazy val sprinter_type   = Project(id = "sprinter-type",      base = file("components/typePrinter"), settings = defaults ++ compilerSettings ++ Seq(name := "sprinter-type")) dependsOn(sprinter_tree)
  lazy val sprinter_test = Project(id = "sprinter-test",        base = file("components/testPrinter"), settings = defaults ++ compilerSettings ++ testSettings ++ Seq(name := "sprinter-test") ++ Seq(publishArtifact := false)) dependsOn(sprinter_tree, sprinter_type)
}
