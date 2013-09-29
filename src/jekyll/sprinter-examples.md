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


