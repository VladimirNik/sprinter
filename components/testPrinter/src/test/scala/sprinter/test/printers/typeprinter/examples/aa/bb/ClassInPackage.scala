package aa.bb.cc.dd

trait C[T] {
  def ccTest[U >: T](x: Int)(y: U, z: List[T]): T
  val cTest: Int
}


trait H extends C[String] {
  def ccTest(x: Int)(y: String, z: List[String]) = 4
}