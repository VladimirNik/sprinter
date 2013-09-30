---
layout: default
title: Sprinter Example
---

# Examples

**1. Hello world program**

For tree:
<br>

    PackageDef(
      Select(Ident(newTermName("hello")), newTermName("world")), 
      List(
        ModuleDef(Modifiers(), newTermName("Main"), 
          Template(List(Select(Ident(scala), newTypeName("AnyRef"))),
            emptyValDef, 
            List(
              DefDef(Modifiers(), nme.CONSTRUCTOR, List(), 
                List(List()), TypeTree(), 
                Block(List(Apply(Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR), List())), 
                Literal(Constant(())))), 
                  DefDef(Modifiers(), newTermName("main"), List(), 
                    List(List(ValDef(Modifiers(PARAM), newTermName("args"), 
                    AppliedTypeTree(Ident(newTypeName("Array")), List(Ident(newTypeName("String")))), EmptyTree))), 
                    Select(Ident(scala), newTypeName("Unit")), 
                    Apply(Ident(newTermName("println")), 
                    List(Literal(Constant("Hello, world!"))))))
          )
        )
      )
    )

we'll have the following generated source (version 0.2.0):

    package hello.world {
      object Main  {
        def main(args: Array[String]): scala.Unit = println("Hello, world!")
      }
    }

**2. Annotations**

Original code:
<br>

    @scala.annotation.implicitNotFound(msg = "The method requires an implicit in scope")
    trait KeyedEntityDef{
      def getId(a: String): Int
    }

Generated:

    @scala.annotation.implicitNotFound(msg = "The method requires an implicit in scope") 
    abstract trait KeyedEntityDef  {
      def getId(a: String): Int
    }

**3. Byname parameters**

Original code:
<br>

    class ByNameTest{
      def test(x: => Int) = x
      val fun: (Int, => Double) => String = null
    }

Generated:

    class ByNameTest  {
      def test(x: => Int) = x;
      val fun: ((Int, => Double) => String) = null
    }

**4. Inheritance**

Original code:
<br>

    class A1 ()(x: Int)

    class C1 ()(x: Int)() extends A1 ()(3) {
      def this(y: String) = {
        this()(3)()
      }
    }

Generated:

    class A1 () (private[this] val x: Int) ;
    class C1 () (private[this] val x: Int) () extends A1()(3) {
      def this(y: String) = {
        this()(3)();
        ()
      }
    }

**5. Traits with self-types**

Original code:
<br>

    trait TestTrait {
      self: KeyedEntity[_] =>
      protected val versionNumber = 0.2
    }

Generated:

    abstract trait TestTrait  { self: (KeyedEntity[_$1] forSome {
      type _$1 >: _root_.scala.Nothing <: _root_.scala.Any
        }) =>
      protected val versionNumber = 0.2
    };
    abstract trait KeyedEntity[K >: _root_.scala.Nothing <: _root_.scala.Any]
