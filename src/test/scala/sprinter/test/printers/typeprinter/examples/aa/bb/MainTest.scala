package aa.bb

package aa.bb

//import aa.bb.{Test => UTest}
//import scala.collection.Map
import scala.collection
import scala.collection.mutable.Map
import scala.collection.immutable._

package ee.cc{
  class Test
}
trait A1
trait A2

import ee.cc._
import aa.bb

class X{
  val abc: Test with A1 with A2{val a: Int; def x: Test}  = new Test with A1 with A2 {
    val a = 3
    def x = new Test()
  }
}