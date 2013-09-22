scala-pretty-printer
====================

Tool for generating source code from Scala ASTs.

Can be used to:

 - increase macros support in the IDE 
 - simplify macro debugging
 - generate code from transformed trees

### Build process

To publish library to local repo run (from project's root directory):

```shell
$ sbt publish-local
```

Target jar should have similar path:

```shell
/path/to/.ivy2/local/org.scala-lang/scala-pretty-printer_2.10/0.2.0/jars/scala-pretty-printer_2.10.jar
```

See <http://scala-sbt.org/release/docs/Getting-Started/Setup.html> for instructions to setup sbt.

### Usage

#### Sbt projects:

In the target project add to project's build file following options:

```scala
libraryDependencies ++= Seq("org.scala-lang" % "scala-compiler" % "2.10.2",
	"org.scala-lang" %% "scala-pretty-printer" % "0.2.0")
```

If you use scala-pretty-printer as a dependency for compiler plugin it's required to add lib's jar to scalac toolcp or include printer's classes to project's jar.

In the case of sbt-based project you can use assembly plugin - <https://github.com/sbt/sbt-assembly> to create common jar. 

Example can be found in printPlugin's build file - <https://github.com/VladimirNik/printPlugin/blob/master/project/SourcePrinter.scala>.

#### Standalone projects:

For default projects to add scala-pretty-printer's jar to scalac toolcp you can use compiler's toolcp option:

```shell
$ scalac -toolcp /path/to/jar/scala-pretty-printer_2.10.jar -Xplugin:/path/to/plugin/printplugin-2.10.jar hello/world/*.scala
```

#### API:

To use pretty-printer import PrettyPrinters class:

```scala
import scala.pretty.printers.PrettyPrinters
```

Create its instance (using object of type nsc.Global):

```scala
val printers = PrettyPrinters(global)
```

and then pass to printers show method required AST:

```scala
printers.show(tree)
```

### Results example

For tree:

```shell
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
```

we'll have the following generated source (version 0.2.0):

```scala
package hello.world {
  object Main  {
    def main(args: Array[String]): scala.Unit = println("Hello, world!")
  }
}
```

### Resources

 - <https://github.com/VladimirNik/printPlugin> - plugin that uses scala-pretty-printer to regenerate project's sources based on ASTs.
 - <https://github.com/VladimirNik/treePrintTester> - example of printPlugin usage.
 - <http://scala-sbt.org/release/docs/Getting-Started/Setup.html> - sbt setup
