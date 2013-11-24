package sprinter.test.printers.typeprinter

import scala.tools.nsc
import java.io.{File, StringWriter, PrintWriter}
import scala.tools.nsc.interactive.Response
import scala.sprinter.printers.{PrettyPrinters, TypePrinters}
import scala.tools.refactoring.Refactoring
import scala.tools.refactoring.util.CompilerProvider
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.refactoring.common.CompilerAccess
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

  object TestRef extends Refactoring with CompilerProvider with scala.tools.refactoring.common.InteractiveScalaCompiler {
    import global._

    val file = new BatchSourceFile(randomFileName(), sourceStr)
    val tree = global.parseTree(file)
//    val tree = treeFrom(sourceStr)

    def cleanTree(t: global.Tree) = {
      global.ask{ () =>
        val removeAuxiliaryTrees = ↓(transform {

          case t: global.Tree if (t.pos == global.NoPosition || t.pos.isRange) => t

          case t: global.ValDef => global.emptyValDef

          // We want to exclude "extends AnyRef" in the pretty printer tests
          case t: global.Select if t.name.isTypeName && t.name.toString != "AnyRef" => t

          case t => global.EmptyTree
        })

        (removeAuxiliaryTrees &> topdown(setNoPosition))(t).get
      }
    }

    def generatePrint(tree: Tree, changeset: ChangeSet = AllTreesHaveChanged, sourceFile: Option[scala.reflect.internal.util.SourceFile]): String = {

      val initialIndentation = if(tree.hasExistingCode) indentationString(tree) else ""
      val in = new Indentation(defaultIndentationStep, initialIndentation)

      //      scala.sprinter.printers.PrettyPrinters.apply(global).show(tree)
      print(tree, PrintingContext(in, changeset, tree, sourceFile)).asText
    }

    def print(tree: global.Tree): String = {
//      val res = generatePrint(cleanTree(tree), sourceFile = None)
            val res = generatePrint(tree, sourceFile = None)
      res
    }

    def shutdown() =
      global.askShutdown()
  }

  trait TestGlobalSettings extends Refactoring with CompilerAccess {
    val global: nsc.Global
    import global._

    def cleanTree(t: global.Tree) = {
//              global.ask{ () =>
      val removeAuxiliaryTrees = ↓(transform {

        case t: global.Tree if (t.pos == global.NoPosition || t.pos.isRange) => t

        case t: global.ValDef => global.emptyValDef

        // We want to exclude "extends AnyRef" in the pretty printer tests
        case t: global.Select if t.name.isTypeName && t.name.toString != "AnyRef" => t

        case t => global.EmptyTree
      })

      (removeAuxiliaryTrees &> topdown(setNoPosition))(t).get
      //        }
    }

    def compilationUnitOfFile(f: AbstractFile): Option[global.CompilationUnit] = Option(global.currentUnit)

    def generatePrint(tree: Tree, changeset: ChangeSet = AllTreesHaveChanged, sourceFile: Option[scala.reflect.internal.util.SourceFile]): String = {

      val initialIndentation = if(tree.hasExistingCode) indentationString(tree) else ""
      val in = new Indentation(defaultIndentationStep, initialIndentation)

//            scala.sprinter.printers.PrettyPrinters.apply(global).show(tree)
      print(tree, PrintingContext(in, changeset, tree, sourceFile)).asText
    }

    def print(tree: global.Tree): String = {
//            val res = generatePrint(cleanTree(tree), sourceFile = None)
      val res = generatePrint(tree, sourceFile = None)
      res
    }
  }

  trait TestInterGlobalSettings extends TestGlobalSettings {
    val global: nsc.interactive.Global

    override def cleanTree(t: global.Tree) = {
      global.ask{ () =>
        super.cleanTree(t)
      }
    }

    def shutdown() =
      global.askShutdown()
  }

  object TestGlobal extends TestGlobalSettings {
    val global = getCompiler
  }

  object TestInterGlobal extends TestInterGlobalSettings {
    val global: nsc.interactive.Global = getInteractiveCompiler(getCompiler)
  }

  def main(args: Array[String]) {
    val compiler = TestGlobal.global

    println("baseDir: " + baseDir)
    println("testPath: " + testPath)

    val sourceFiles = List(
      s"${testPath}aa${col}bb${col}MainTest.scala",
      s"${testPath}aa${col}bb${col}cc${col}dd${col}OtherClass.scala"
    )

    val allSources = sourceFiles map {
      fn => compiler.getSourceFile(fn)
    }
    val interactive = TestInterGlobal.global
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

//    import interactive._

    var lTree: compiler.Tree = null
    loadedResponse.get match {
      case Right(e) => System.out.println("Failed to get tree in PRINT PLUGIN")
        "Error during getting interactive compiler"
      case Left(_) => {
        val loadedTree: interactive.Tree = loadedResponse.get.left.get
        lTree = loadedTree.asInstanceOf[compiler.Tree]
      }
    }

    val pTree = interactive.parseTree(allSources(0))
//
//        val typeTrees = (loadedTree.filter{
//          case interactive.ValDef(mods, name, tp, rhs) => true
//          case _ => false
//        }).asInstanceOf[List[ValDef]].distinct.map(_.tpt)
//
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
        System.out.println(PrettyPrinters.apply(TestRef.global).show(pTree))
//        System.out.println(TestRef.print(pTree.asInstanceOf[TestRef.global.Tree]))
    TestRef.shutdown()
    TestInterGlobal.shutdown()

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
