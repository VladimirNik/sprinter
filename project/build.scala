import sbt._
import Keys._
import com.typesafe.sbt.SbtSite.site
import com.typesafe.sbt.SbtSite.SiteKeys._
import com.typesafe.sbt.site.JekyllSupport.Jekyll
import com.typesafe.sbt.SbtGhPages.ghpages
import com.typesafe.sbt.SbtGit.git

object SprinterBuild extends Build {
   val sprinter = Project("sprinter", file(".")) settings (
    organization := "org.scala-lang",
    name := "sprinter",
    version := "0.2.0",
    scalaVersion := "2.10.2",
    //scalaBinaryVersion <<= scalaVersion,
    //crossVersion := CrossVersion.full,
    //exportJars := true,
    libraryDependencies <++= scalaVersion apply dependencies,
    publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))
  ) settings (websiteSettings: _*)

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

  def dependencies(sv: String) = Seq(
    "org.scala-lang" % "scala-compiler" % sv) 
}
