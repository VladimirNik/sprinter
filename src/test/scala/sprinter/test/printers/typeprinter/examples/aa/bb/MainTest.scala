package aa.bb

package object bb {
  val a = 5
  println("hello world")
}
//

object `package` {
  private[this] val a = 5
  scala.Predef.println("hello world")
  class NNN
  class BBB {
    private[this] val b = `package`.this.a
  }
}


class Another(x: String) {
  def t = 7
}

case class CaseTest

trait HHH {
  val a: Int
  val k: Map[Int, FFF] = Map[Int, FFF]().empty
}

trait YYY[T, U] {
  def fff[Z](x: T, H: Z): (String, U)
}

trait VVV

abstract class FFF {
  self: HHH with VVV =>
  def a = 7
  def b
  val g: Int
}

class TestClass(val x: Int, var y: String)(g: String) extends Another(y){
//  override val t = 4
  type Rep

  def zzz[T >: VVV, U[R[I,M]]](x: Int=> Double =>FFF)(v: =>Double) = ";;"
  import scala.collection.mutable.{Map => UMap, _}
  val xs: FFF{val x: Int
  var e: Int} = null

  val q @ (xq, yq) = (1,2)

  val e = ""
  def this(x: String) = this(4,x)(x)

  def this() {
    this(5, "tt")("gg")
    super.t
  }
//  val a = 5
  val f = super.t
  class F {
    val f = 7
    val nn = f
    def test[T](x: Int): T = x.asInstanceOf[T]
    val dd = TestClass.this.f
  }
//  def b(v: String) = v
//  val c = this.x
//  class F{
//    val d = {
//      TestClass.this.x
//    }
//  }
//  def aa: Int = 5
//  def bb = 7
//  private[this] val cc = bb
  val ll = List(1,2,3)
  ll match {
    case l@List(a, _*) => true
    case _ => false
  }

  class Presuper extends {
    protected val a = 5
    val n = 9
    type X = String
  } with VVV
  class BBB extends YYY[String, Int] {
    val z = 5
    override def fff[I](b: String, n: I): (String, Int) = ("", ().asInstanceOf[Int])
  }

  class NNN extends VVV

}