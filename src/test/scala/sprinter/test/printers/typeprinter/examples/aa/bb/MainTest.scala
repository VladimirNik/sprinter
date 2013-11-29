package aa.bb

package aa.bb.cc.dd

class Another(x: String) {
  def t = 7
}

case class CaseTest

trait HHH {
  val a: Int
}

abstract class FFF {
  def a = 7
  def b
  val g: Int
}

class TestClass(val x: Int, var y: String)(g: String) extends Another(y){
//  override val t = 4
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
}