package aa.bb

//import aa.bb.cc.dd.ClassInPackage
//import aa.bb.cc.dd.OtherClass
//import scala.collection.mutable
//import scala.collection

//import aa.bb.cc.{dd => hh}
import scala.collection.immutable.Map
//import aa.bb.cc.dd.G.{GType => NNN}
//import aa.bb.cc.dd.G
import aa._

class OtherClass

/*
object G {
  type GType = String
  val a: String = "dd"
}
*/

import aa.bb.cc.dd.G.{GType => FType}
import aa.bb.cc.dd.{G => CHANGE}
//import aa.bb.cc.dd.G.{GType => FType}

class MainClass {
  //val a: String = "hhh"
  //import scala.collection.mutable._
  //val b: Int = 5
  //()
  //val c: String = "ggg"
  //import scala.collection.mutable.Map
  import scala.collection.mutable.Map
  import scala.collection.mutable.Map
  //import scala.collection.mutable._
  //import scala.collection.mutable._
  import scala.collection._
  /* {
    import scala.collection
    import scala._
    //import scala.collection._
    //val z: scala.collection.mutable.Map[Int, Int] = null //mutable map
  } */
  import aa.bb.ClassInPackage
  import aa.bb.ClassInPackage
  //import aa.bb.cc.dd.G
  //val zzz: OtherClass = null
  //val ddd: ClassInPackage = null
  //G.a
  //val ggg: aa.bb.G.type = null
  //import aa.bb.cc.dd.G.GType
  //import aa.bb.cc.dd.G
  //import aa.bb.cc._
  val a: CHANGE.GType = null
  val test: scala.collection.immutable.Map[Map[Int, aa.bb.cc.dd.G.GType], CHANGE.GType] = null
  val test2: immutable.Map[Map[Int,FType],FType] = null
  //val a: dd.G.GType = null
  //val zm: scala.collection.mutable.Map[Int, Int] = null
  //val zm: mutable.Map[Int, Int] = null
  //val bo: Object = null
  //val sm: Manifest[Int] = null

  //val (fa, fy) = (5, "ggg")
//
  //val List(first, _*) = List(1,2,3)

  val z: List[Int] = null
  val f = List(1,2,3)
  z match {
     case Nil => println("1")
     case List(x) => x
     case List(x,y) => y
     case List(x,y,z) => z
     case List(x, _*) => x
     case _ =>
  }
  case class XY
} 

/*
class FF {
  val x: Int = 3
}
*/
