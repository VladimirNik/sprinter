package scala.sprinter.test
import org.junit.Test
import org.junit.Assert._
import reflect.PrettyPrinter
import scala.tools.reflect._
import scala.reflect.runtime.universe.{show=>_,_}
import scala.reflect.runtime.{currentMirror=>cm}
import PrettyPrinter._
object testHelpers{
  val toolbox = cm.mkToolBox()
  def assertStableNormalized(code: String,tree: Tree) = {
    val toolboxTree = 
      try{
        toolbox.parse(code)
      } catch {
        case e:scala.tools.reflect.ToolBoxError => throw new Exception(e.getMessage + ": " + code)
      }
    def normalize( str:String ) = str.split("(\r\n|\n\r|\r|\n)").map(_.trim).mkString("\n")
    assertEquals( "using quasiquote or given tree"+"\n"/*+showRaw(tree)+"\n"+showRaw(toolboxTree)*/, code, show(tree).trim )
    assertEquals( "using toolbox parser", code, show(toolboxTree).trim )
  }
}
import testHelpers._
class TestSymbolic{
  @Test def testIdent = assertStableNormalized( "*", Ident("*") )
  @Test def testIdent2 = assertStableNormalized( "*", q"*" )
  @Test def testConstant = assertStableNormalized( "\"*\"", Literal(Constant("*")) )
  @Test def testVal = assertStableNormalized( "val * : Unit = null", q"val * : Unit = null" ) // val *: Unit does not compile
  @Test def testDef = assertStableNormalized( "def * : Unit = null", q"def * : Unit = null" )
  @Test def testTrait = assertStableNormalized( "trait *", q"trait *" )
  @Test def testClass = assertStableNormalized( "class *", q"class *" )
  @Test def testClassWithPublicParams = assertStableNormalized( "class xXx(val x: Int, val s: String)", q"class xXx(val x: Int, val s:String)" )
  //FIXME: @Test def testClassWithParams = assertStableNormalized( "class xXx(x: Int, s:String)", q"class xXx(x: Int, s:String)" )
  @Test def testObject = assertStableNormalized( "object *", q"object *" )
  @Test def testObjectWithBody = assertStableNormalized( """
object X {
  def y = "test"
}
""".trim, q"""object X{ def y = "test" }""" )
  @Test def testClassWithBody = assertStableNormalized( """
class X {
  def y = "test"
}
""".trim, q"""class X{ def y = "test" }""" )
  @Test def testTraitWithBody = assertStableNormalized( """
trait X {
  def y = "test"
}
""".trim, q"""trait X{ def y = "test" }""" )
  @Test def testTraitWithSelfTypeAndBody = assertStableNormalized( """
trait X { self: Order =>
  def y = "test"
}
""".trim, q"""trait X{ self: Order => def y = "test" }""" )
  @Test def testTraitWithSelf = assertStableNormalized( """
trait X { self =>
  def y = "test"
}
""".trim, q"""trait X{ self => def y = "test" }""" )
  @Test def testCaseClassWithBody = assertStableNormalized( """
case class X {
  def y = "test"
}
""".trim, q"""case class X{ def y = "test" }""" )
/* FIXME:
  @Test def testCaseClassWithParamsAndBody = assertStableNormalized( """
case class X(x: Int, s:String) {
  def y = "test"
}
""".trim, q"""case class X(x: Int, s:String){ def y = "test" }""" )
*/
}

class TestEmpty{
  @Test
  def testEmpty = assertEquals( "", show(EmptyTree) )
  def testToString = assertEquals( "<empty>", EmptyTree.toString ) // PrettyPrinter relies on this assumption
}