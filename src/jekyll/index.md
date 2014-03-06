---
layout: default
title: sprinter
---

Welcome to the Sprinter project! 
<br>
<br>
Sprinter is a library for generating correct Scala code from Scala abstract syntax trees (ASTs). The library reverts desugaring performed by the Scala parser. Currently, Sprinter desugars class and method definitions, traits, constructors and other language constructs. Example of generated code for Hello world program is presented below:

    package hello.world {
      object Main  {
        def main(args: Array[String]): scala.Unit = println("Hello, world!")
      }
    }

Sprinter currently generates correct Scala code from untyped ASTs. This is verified by regenerating source code
of numerous community projects and successfully executing their test suites. The list of project includes
[Shapeless](https://github.com/milessabin/shapeless), [Scalaz](https://github.com/scalaz/scalaz),
[Akka](http://akka.io), [Squeryl](http://squeryl.org/), etc. Future versions will improve typed Scala ASTs printing and
will revert transformations performed by the Scala typer phase.
 
Sprinter can be used in many different contexts. The most important use cases are:

 * Showing macro expansions in IDEs. For example, users will be able to expand macro invocations to see
generated code.
 * Enable debugging of macros by generating source code with all macros expanded.
 * Debugging and error messages in the scalac compiler.
 * Generate source code from trees before macro annotations arrive.
 * Generate source of refactored code.

Library is in active development. All ideas, bug reports, and contributions are very welcome. If you have an
interesting use case for the library do not hesitate to contact the authors. 

The road map for Sprinter is:

 1. Improve code generation for typed Scala ASTs.
 2. Integrate with the Scala IDE to provide macro expansion and debugging.
 3. Beautify the code by removing unnecessary qualifiers (type prefixes).
 4. Integrate the library into `scalac` for debugging and nicer error messages.

Beginning with 2.11-RC1 Sprinter's tree-based code generator is a part of Scala.

<br>

The [Guide](sprinter-guide.html) will explain how to use Sprinter. In [Examples](sprinter-examples.html) you can see code snippets generated with Sprinter. Sprinter's code generator can be found [here](https://github.com/VladimirNik/scala/blob/typedTreesPrinter-2.11.0/src/reflect/scala/reflect/internal/Printers.scala#L538). Original project [repository](https://github.com/vladimirnik/sprinter) (before Scala 2.11).
