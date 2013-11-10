package sprinter.test.printers.typeprinter

import scala.tools.nsc
import java.io.{StringWriter, PrintWriter}
import scala.tools.nsc.interactive.Response
import scala.sprinter.printers.TypePrinters

object CompilerRunner {
  def main(args: Array[String]) {
    val compiler = getCompiler

//    println("compiler.settings = " + compiler.settings)

    val sourceFiles = List(
      "/home/vova/scala-projects/GSoC/type-printing/sprinter/src/test/scala/sprinter/test/printers/typeprinter/examples/aa/bb/MainTest.scala",
      "/home/vova/scala-projects/GSoC/type-printing/sprinter/src/test/scala/sprinter/test/printers/typeprinter/examples/aa/bb/cc/dd/OtherClass.scala"
    )

    val allSources = sourceFiles map {
      fn => compiler.getSourceFile(fn)
    }
    val interactive = getInteractiveCompiler(compiler)
    val typePrinters = TypePrinters(interactive)
    val response = new Response[Unit]()

    interactive.askReload(allSources, response)
    response.get match {
      case Left(ret) => println("Sources loaded")
      case Right(e) => println("Error while loading sources"); return
      case _ => println("Something wrong"); return
    }

    val loadedResponse = new Response[interactive.Tree]
    interactive.askLoadedTyped(allSources(0), loadedResponse)

    import interactive._

    loadedResponse.get match {
      case Right(e) => System.out.println("Failed to get tree in PRINT PLUGIN")
        "Error during getting interactive compiler"
      case Left(_) => {
        val loadedTree: interactive.Tree = loadedResponse.get.left.get

        val typeTrees = (loadedTree.filter{
          case interactive.ValDef(mods, name, tp, rhs) => true
          case _ => false
        }).asInstanceOf[List[ValDef]].distinct.map(_.tpt)

        val resultInfo = interactive.askForResponse(
          () =>
            typeTrees.map{
              tt =>
                val context = interactive.locateContext(tt.pos)
                val result = typePrinters.showType(tt, context.get)
                System.out.println("RESULT (PRINT_PLUGIN) = " + result)
                //add list of imports
                //add first type
                System.out.println(s"(tt: $tt, context: $context)")
                (tt, context)
            }
        )

        resultInfo.get match {
          case Left(value) =>
            value.foreach{
              case (tree, Some(context)) =>
                val result = typePrinters.showType(tree, context)
                System.out.println("RESULT (PRINT_PLUGIN) = " + result)
              case (tree, None) => System.out.println(s"Context for $tree is not found")
            }
          case Right(_) => System.out.println("Test is not workable")
        }

        //TODO try context after askShutdown
      }
      interactive.askReset()
      interactive.askShutdown()
    }
  }

  def getInteractiveCompiler(global: nsc.Global) =
    new nsc.interactive.Global(global.settings, global.reporter)

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
