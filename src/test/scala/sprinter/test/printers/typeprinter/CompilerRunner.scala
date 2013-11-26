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

//    val pTree = interactive.parseTree(allSources(0))

    val typeTrees = (lTree.filter{
      case cd: ClassDef if cd.symbol.isAbstractClass && !cd.symbol.isTrait => true
      case _ => false
    })

    val abstractTree = typeTrees(0)

    println("Abstract Class: " + abstractTree)
    println("=================")

    val result = interactive.askForResponse {
      () =>
        val atType = abstractTree.symbol.tpe
        println("abstractTree.symbol.tpe: " + abstractTree.symbol.tpe)
        println("abstractTree.symbol.tpe.resultType: " + abstractTree.symbol.tpe.resultType)
        println("abstractTree.symbol.tpe.resultType.underlying: " + abstractTree.symbol.tpe.resultType.underlying)
        println("abstractTree.tpe: " + abstractTree.tpe)
        println("abstractTree.tpe.resultType: " + abstractTree.tpe.resultType)
        println("abstractTree.tpe.resultType.underlying: " + abstractTree.tpe.resultType.underlying)
        println("--------------------")

        val members = atType.underlying.members
        members foreach {
          m =>
            println("method - " + m.isMethod + ": " + interactive.show(m))
            println("m.isIncompleteIn(atType.termSymbol): " + m.isIncompleteIn(atType.termSymbol))
            println("m.isDeferred: " + m.isDeferred)
            println("m.isVal: " + m.isVal) //false for all
            println("m.isVal: " + m.isVar) //false for all
            println("m.isValue: " + m.isValue) //true for val/var
            println("m.isVariable: " + m.isVariable) //false for all
            println("m.isSetter: " + m.isSetter)
            println("m.isGetter: " + m.isGetter)

            println("---")
            println("m.paramss: " + m.paramss)
            println("m.typeParams: " + m.typeParams)
            println("m.typeOfThis: " + m.typeOfThis)
            println("m.owner: " + m.owner)
            println("m.typeOfThis.asSeenFrom(abstractTree.symbol.tpe, m.owner): " + m.typeOfThis.asSeenFrom(abstractTree.symbol.tpe, m.owner))

            println("---------------------")
            "finished"
        }
        val nonDefinedMembers = members filter (m => (m.isMethod || m.isValue) && m.isIncompleteIn(atType.termSymbol) && m.isDeferred && !m.isSetter)
    }

    result.get match {
      case Left(value) => println("Successfully finished")
      case Right(_) => println("error")
    }

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

    System.out.println("Here is result: ")
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