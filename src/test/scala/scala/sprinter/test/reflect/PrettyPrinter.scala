package scala.sprinter.test.reflect
import scala.reflect.api.Universe
import scala.sprinter.printers.PrettyPrinters

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import java.io._

/** adapted from @amirsh */
object PrettyPrinter {

  val global: Global = {
    val settings = new Settings()

    val COLON = System getProperty "path.separator"

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(COLON)
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(COLON)
      case _ => System.getProperty("sun.boot.class.path")
    }

    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""

    val reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out)) //writer
    new Global(settings, reporter)
  }

  val printers = PrettyPrinters(global)

  def show(tree: Universe#Tree): String = {
    printers.show(tree.asInstanceOf[Global#Tree], PrettyPrinters.AFTER_NAMER, printMultiline = true, decodeNames = true)
  }

  def showNsc(tree: Universe#Tree): String = {
    val baos = new ByteArrayOutputStream
    val ps = new PrintStream(baos)
    scala.Console.withOut(ps){
      global.treePrinter.print(tree)
      baos.toString
    }
  }
}