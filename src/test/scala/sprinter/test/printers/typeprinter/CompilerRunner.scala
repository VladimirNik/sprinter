package sprinter.test.printers.typeprinter

import scala.tools.nsc
import java.io.{File, StringWriter, PrintWriter}
import scala.tools.nsc.interactive.Response
import scala.sprinter.printers.{PrettyPrinters, TypePrinters}
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.AbstractFile
import scala.reflect.internal.MissingRequirementError

//TODO reimplement and create tests
object CompilerRunner {
  val baseDir: String = System.getProperty("user.dir")
  val col = File.separator
  val testPath = s"${baseDir}${col}src${col}test${col}scala${col}sprinter${col}test${col}printers${col}typeprinter${col}examples${col}"

  val sourceStr = """
      package aa.bb.cc.dd
        class X
      trait YYY {
        protected def aaa: Int
      }

      trait ZZZ extends YYY {
       abstract override protected def aaa: Int = 5
      }

      abstract protected class Test(protected[dd] val x: Int, var z: Double)(protected[dd] val ff: Int)(implicit val y: Float, protected[dd] var m: String) {

        implicit val z: List[Int] = null
        val f = List(1,2,3)

        protected def a(x: String)(implicit v: Int): Int = 5
        protected[this] val x: Map[Int, Int] = null
      }

      abstract class ZZZ(implicit val n: Int = 5)

      trait EEE {
        val c = new ZZZ{}
        implicit val m = c.n
        def bbb = {
          implicit val yyy = 5
          5
        }
      }
                  """

  def main(args: Array[String]) {
    val interactive = getInteractiveCompiler(getCompiler)

    val file = new BatchSourceFile("testFile", sourceStr)
    val tree = interactive.parseTree(file)

    val printer = PrettyPrinters(interactive)
    System.out.println("Result: " + printer.show(tree))
    interactive.askShutdown()
  }

  def getInteractiveCompiler(global: nsc.Global) = {
    val comp = new nsc.interactive.Global(global.settings, global.reporter)
    try {
      comp.ask { () =>
        new comp.Run
      }
    } catch {
      case e: MissingRequirementError =>
        val msg = s"""Could not initialize the compiler!""".stripMargin
        throw new Exception(msg, e)
    }
    comp
  }

  def getCompiler = {
    import scala.tools.nsc._
    import scala.tools.nsc.reporters._
    val settings = new Settings()

    val COLON = System getProperty "path.separator"

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(COLON)
      case _                            => System.getProperty("java.class.path")
    }

    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(COLON)
      case _                            => System.getProperty("sun.boot.class.path")
    }

    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""

    val reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out)) //writer
    new scala.tools.nsc.Global(settings, reporter)
  }
}