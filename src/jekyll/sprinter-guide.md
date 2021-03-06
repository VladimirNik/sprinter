---
layout: default
title: Sprinter Guide
---

# Sprinter Guide

## Usage (Scala 2.11.x)
Pass the tree to showCode method from universe:

	import scala.reflect.runtime.universe
	val tree = ... // unattributed or attributed AST
	universe.showCode(tree)

## Build process (Scala 2.10.x)
All information below is relevant for Scala 2.10.x.
To publish library to local repo run (from project's root directory):

    $ sbt publish-local

Target jar should have similar path:

    /path/to/.ivy2/local/org.scala-lang/sprinter_2.10/0.2.0/jars/sprinter_2.10.jar

See [sbt pages](http://scala-sbt.org/release/docs/Getting-Started/Setup.html) for instructions to setup sbt.

## Usage
####1. Sbt projects:

In the target project add to project's build file following options:

    libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % "2.10.2",
	  "org.scala-lang" %% "sprinter" % "0.2.0")

If you use sprinter as a dependency for compiler plugin it's required to add lib's jar to scalac toolcp or include printer's classes to project's jar.

In the case of sbt-based project you can use [assembly plugin](https://github.com/sbt/sbt-assembly) to create common jar. 

Example can be found in printPlugin's [build file](https://github.com/VladimirNik/printPlugin/blob/master/project/SourcePrinter.scala).

####2. Standalone projects:

For default projects to add sprinter's jar to scalac toolcp you can use compiler's toolcp option:

    $ scalac -toolcp /path/to/jar/sprinter_2.10.jar -Xplugin:/path/to/plugin/printplugin-2.10.jar hello/world/*.scala

## API
To use sprinter import PrettyPrinters class:

    import scala.sprinter.printers.PrettyPrinters

Create its instance (using object of type nsc.Global):

    val printers = PrettyPrinters(global)

and then pass to printers show method required AST:

    printers.show(tree)

## Resources

 - [printPlugin](https://github.com/VladimirNik/printPlugin) - plugin that uses sprinter to regenerate project's sources based on ASTs.
 - [treePrintTester](https://github.com/VladimirNik/treePrintTester) - example of printPlugin usage.
 - [sbt setup](http://scala-sbt.org/release/docs/Getting-Started/Setup.html)

