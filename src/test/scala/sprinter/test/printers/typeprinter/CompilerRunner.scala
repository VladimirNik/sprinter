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
      case class Test {
        import scala.collection.mutable
//
//        val (x, y) = (5, "ggg")
//
//        val List(a, _*) = List(1,2,3)
//
//        val z: List[Int] = null
//        val f = List(1,2,3)
//        z match {
//          case Nil => println("1")
//          case List(x) => x
//          case List(x,y) => y
//          case List(x,y,z) => z
//          case List(x, _*) => x
//          case _ =>
//        }
//
        val x: mutable.Map[Int, Int] = null
      }
"""

  def main(args: Array[String]) {
    val compiler = getCompiler
    val interactive = getInteractiveCompiler(getCompiler)

    val file = new BatchSourceFile("testFile", sourceStr)
    val tree = interactive.parseTree(file)

    println("baseDir: " + baseDir)
    println("testPath: " + testPath)

    val sourceFiles = List(
      s"${testPath}aa${col}bb${col}MainTest.scala",
      s"${testPath}aa${col}bb${col}cc${col}dd${col}OtherClass.scala"
    )

    val allSources = sourceFiles map {
      fn => compiler.getSourceFile(fn)
    }
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

    var lTree: Tree = loadedResponse.get match {
      case Right(e) => throw new Exception("Error during getting interactive compiler")
      case Left(_) => {
        loadedResponse.get.left.get
      }
    }

    val result = PrettyPrinters(interactive).show(lTree, PrettyPrinters.AFTER_TYPER)
    System.out.println(result)

//    val pTree = interactive.parseTree(allSources(0))

//    val typeTrees: List[ClassDef] = (lTree.filter{
//      case cd: ClassDef if cd.symbol.isAbstractClass && !cd.symbol.isTrait => true
//      case _ => false
//    }).asInstanceOf[List[ClassDef]]
//
//    val abstractTree = typeTrees(0)
//
//    println("Abstract Class: " + abstractTree)
//    println("=================")
//
//    val result = interactive.askForResponse {
//      () =>
//        val atType = abstractTree.symbol.tpe
//        val tdev = (lTree.filter { _ match {
//          case td:DefDef => true
//          case _ => false
//        }}).last
//    }
//
//    result.get match {
//      case Left(value) => println("Successfully finished")
//      case Right(_) => println("error")
//    }

//        val resultInfo = interactive.askForResponse(
//          () =>
//            typeTrees.map{
//              tt =>
//                val context = interactive.locateContext(tt.pos)
//                val result = typePrinters.showType(tt, context.get)
//                System.out.println("RESULT (PRINT_PLUGIN) = " + result)
//                //add list of imports
//                //add first type
//                System.out.println(s"(tt: $tt, context: $context)")
//                (tt, context)
//            }
//        )
//
//        resultInfo.get match {
//          case Left(value) =>
//            value.foreach{
//              case (tree, Some(context)) =>
////                val result = typePrinters.showType(tree, context)
////                System.out.println("RESULT (PRINT_PLUGIN) = " + result)
//                System.out.println("Executed successfully!!!")
//              case (tree, None) => System.out.println(s"Context for $tree is not found")
//            }
//          case Right(_) => System.out.println("Test is not workable")
//        }
//
//        //TODO try context after askShutdown
//      }

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


//for typer-based printing
//final def keyString: String =
//if (isJavaInterface) "interface"
//else if (isTrait && !isImplClass) "trait"
//else if (isClass) "class"
//else if (isType && !isParameter) "type"
//else if (isVariable) "var"
//else if (isPackage) "package"
//else if (isModule) "object"
//else if (isSourceMethod) "def"
//else if (isTerm && (!isParameter || isParamAccessor)) "val"
//else ""