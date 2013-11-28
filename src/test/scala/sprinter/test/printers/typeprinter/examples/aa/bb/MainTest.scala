package aa.bb

import aa.bb.cc.dd._

package ee.ff{
  class TestCl
}

trait B {
//  def b: Int
//  var mmm: String
//  val nnn: Int
//  def bb(x: Int)
//  def bbb = 5
}

trait A extends B {
//  def b: Int
//  def a: String
//  def aa(x: String, y: String): (Int, Int)
  def b: Int
}

import ee.ff._

abstract class MainClass extends A with C[TestCl] {
  def test[U,V]
//  def bb(x: Int) = 5

}