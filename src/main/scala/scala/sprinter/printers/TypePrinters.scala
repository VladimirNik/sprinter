package scala.sprinter.printers

import scala.tools.nsc.interactive.Global
import scala.tools.nsc
import java.io.{StringWriter, PrintWriter}
import scala.reflect.internal.util.SourceFile

object TypePrinters {
  def apply(compiler: Global) = {
    new PrettyPrinters(compiler) with TypePrinters {
      override val interactive: Global = compiler
    }
  }

  def showType(compiler: nsc.interactive.Global, what: nsc.interactive.Global#Tree, context: nsc.interactive.Global#Context) = {
    apply(compiler).showType(what, context)
  }

  def showType(compiler: nsc.interactive.Global, what: nsc.interactive.Global#Type, context: nsc.interactive.Global#Context) = {
    apply(compiler).showType(what, context)
  }

//  def showType(compiler: nsc.interactive.Global, what: nsc.interactive.Global#Tree) = {
//    val typePrinters = TypePrinters.apply(compiler)
//    val contextInfo = compiler.askForResponse(
//      () =>
//        compiler.locateContext(what.pos)
//    )
//    contextInfo.get match {
//      case Left(value) =>
//        value match {
//          case Some(context) =>
//            typePrinters.showType(what, context)
//          case None => what.toString()
//        }
//      case Right(_) => what.toString()
//    }
//  }
}

trait TypePrinters {
  self: PrettyPrinters =>

  val interactive: Global
  import interactive._

  def showType(what: nsc.Global#Tree, context: nsc.interactive.Global#Context) = {
    val printer = new TypePrinter()
    printer.showTypeTree(what.asInstanceOf[Tree], context.asInstanceOf[Context])
  }

  def showType(what: nsc.Global#Type, context: nsc.interactive.Global#Context) = {
    val printer = new TypePrinter()
    printer.showPrettyType(what.asInstanceOf[Type], context.asInstanceOf[Context])
  }

  class TypePrinter {

    import interactive.shorthands
    import interactive.definitions.{isFunctionType, isTupleType, isByNameParamType, parentsString}
    import interactive.definitions.{ByNameParamClass, RepeatedParamClass}

    def showPrettyType(inType: Type, context: Context): String = {
      lazy val availImports = context.imports
      if (!Option(context).isEmpty && !availImports.isEmpty) {

        def showType(inType: Type): String = {

          inType match {
            case typeRef@TypeRef(pre, sym, args) => {
              def needsPreString = {
                !shorthands(sym.fullName) || (sym.ownersIterator exists (s => !s.isClass))
              }

              def preString(typeName: String, isModuleTypeRef: Boolean = false) = {
                //origPreString ends in .
                val origPreString = pre.prefixString
                //TODO check if origPreString is shorter or empty
                if (!isModuleTypeRef && needsPreString)
                  if (!origPreString.isEmpty) {
                    val avImp = getAvailableImport()
                    avImp match {
                      case Some(imp) => modifyPrefix(origPreString, imp.tree)
                      case None => origPreString
                    }
                  } else origPreString
                else ""
              }

              //TODO fix quoted / unquoted names
              //TODO fix {X => Y} - type should be renamed if we use such import
              //if result.isEmpty - use name from import
              def modifyPrefix(origPrefix: String, im: Import): String = {
                val qual = im.expr.symbol.fullName
                val result =
                  if (!qual.isEmpty)
                    origPrefix.replaceFirst(s"$qual.", "")
                  else origPrefix
                result
              }

              def getAvailableImport() = {
                val impOpt = getImportForSymbol(sym)
                if (impOpt.isEmpty) {
                  getImportForType(pre)
                } else impOpt
              }

              def getImportForType(tp: Type): Option[interactive.analyzer.ImportInfo] = {
                val importOpt = getImportForSymbol(tp.termSymbol)
                importOpt match {
                  case Some(imp) => importOpt
                  case _ if (tp.termSymbol == NoSymbol || tp.termSymbol.isRoot || tp.termSymbol.isRootSymbol) => None
                  case None => getImportForType(tp.prefix)
                }
              }

              def getImportForSymbol(curSymbol: Symbol) = {

                var imports = availImports
                val name = curSymbol.name
                var ambigiousError = false
                var impSym: Symbol = NoSymbol

                //TODO reimplement importedSymbol, and exportedSymbol (to work with T1 => T2)
                while (!impSym.exists && !imports.isEmpty) {
                  impSym = imports.head.importedSymbol(name)
                  if (!impSym.exists) imports = imports.tail
                }

                val resultOpt =
                  if (impSym.exists) {
                    var impSym1: Symbol = NoSymbol
                    var imports1 = imports.tail

                    def ambiguousImport() = {
                      // The types of the qualifiers from which the ambiguous imports come.
                      // If the ambiguous name is a value, these must be the same.
                      def t1  = imports.head.qual.tpe
                      def t2  = imports1.head.qual.tpe
                      // The types of the ambiguous symbols, seen as members of their qualifiers.
                      // If the ambiguous name is a monomorphic type, we can relax this far.
                      def mt1 = t1 memberType impSym
                      def mt2 = t2 memberType impSym1

                      !(impSym.fullName == impSym1.fullName && impSym.name == impSym1.name) && !(mt1.toString() == mt2.toString() && name.isTypeName && impSym.isMonomorphicType && impSym1.isMonomorphicType)
                      //TODO resolve problems with context and JVM instances
                      //!(t1 =:= t2 && impSym.name == impSym1.name) && !(mt1 =:= mt2 && name.isTypeName && impSym.isMonomorphicType && impSym1.isMonomorphicType)
                    }

                    while (!ambigiousError && !imports1.isEmpty &&
                      (!imports.head.isExplicitImport(name) || imports1.head.depth == imports.head.depth)) {
                      impSym1 = imports1.head.importedSymbol(name)
                      if (impSym1.exists) {
                        if (imports1.head.isExplicitImport(name)) {
                          ambigiousError =
                            if (imports.head.isExplicitImport(name) || imports1.head.depth != imports.head.depth)
                              ambiguousImport()
                            else false
                          impSym = impSym1
                          imports = imports1
                        } else if (!imports.head.isExplicitImport(name) &&
                          imports1.head.depth == imports.head.depth) ambigiousError = ambiguousImport()
                      }
                      imports1 = imports1.tail
                    }

                    imports.headOption
                  } else None

                resultOpt match {
                  //TODO - fix and test (problem with =:= and synchronization)
                  case Some(impInfo) if (!ambigiousError && (curSymbol.fullName.startsWith(impInfo.qual.symbol.fullName))) => // && (impInfo.qual.tpe =:= curType.prefix)) =>
                    resultOpt
                  case _ => None
                }
              }

              def isWildcard(s: ImportSelector): Boolean = {
                compareNames(s.name, nme.WILDCARD)
              }

              def argsString = if (args.isEmpty) "" else args.map(tp => showType(tp)).mkString("[", ",", "]")

              def finishPrefix(rest: String) =
                inType match {
                  case rtr: RefinementTypeRef => "" + rtr.thisInfo //TODO - maybe we need to add customToString(rtr.thisInfo)
                  case ptr: PackageTypeRef => "package " + rest //TODO - it's default implementation
                  case mtr: ModuleTypeRef => "object " + rest //TODO - it's default implementation
                  case tr@ TypeRef(pre, sym, args) =>
                    if (sym.isInitialized && sym.isAnonymousClass && !phase.erasedTypes)
                      parentsString(sym.info.parents) + tr.refinementString
                    else rest
                  case _ => inType.toString()
                }

              def customToString = inType.typeSymbol match {
                case RepeatedParamClass => showType(args.head) + "*"
                case ByNameParamClass   => "=> " + showType(args.head)
                case _                  =>
                  def targs = inType.normalize.typeArgs

                  if (isFunctionType(inType)) {
                    // Aesthetics: printing Function1 as T => R rather than (T) => R
                    // ...but only if it's not a tuple, so ((T1, T2)) => R is distinguishable
                    // from (T1, T2) => R.
                    targs match {
                      case in :: out :: Nil if !isTupleType(in) =>
                        // A => B => C should be (A => B) => C or A => (B => C).
                        // Also if A is byname, then we want (=> A) => B because => is right associative and => A => B
                        // would mean => (A => B) which is a different type
                        val showIn = showType(in)
                        val showOut = showType(out)
                        val in_s  = if (isFunctionType(in) || isByNameParamType(in)) "(" + showIn + ")" else "" + showIn
                        val out_s = if (isFunctionType(out)) "(" + showOut + ")" else "" + showOut
                        in_s + " => " + out_s
                      case xs =>
                        xs.init.map(tp => showType(tp)).mkString("(", ", ", ")") + " => " + showType(xs.last)
                    }
                  }
                  else if (isTupleType(inType))
                    targs.map(tp => showType(tp)).mkString("(", ", ", if (hasLength(targs, 1)) ",)" else ")")
                  else if (sym.isAliasType && inType.prefixChain.exists(_.termSymbol.isSynthetic) && (inType ne inType.normalize))
                    "" + showType(inType.normalize)
                  else
                    ""
              }

              //TODO - clean the code
              def safeToString = {
                typeRef match {
                  case mtr: ModuleTypeRef =>
                    val tName = sym.nameString
                    if (sym.isOmittablePrefix) "" else if (tName.isEmpty) typeRef.toString() else preString(tName, true) + tName + "." + "type"
                  case _ => //other TypeRef
                    val custom = customToString
                    if (custom != "") custom
                    else {
                      val typeName = sym.nameString
                      finishPrefix(preString(typeName) + typeName + argsString)
                    }
                }
              }
              //showType(inType: Type): String implementation
              //case typeRef@TypeRef(pre, sym, args) =>
              safeToString
            }
            case _ => inType.toString()
          }
        }
        showType(inType)
      } else inType.toString
    }

    def showTypeTree(tr: Tree, context: Context): String = {
      if (tr.isType) {
        val inType = tr.tpe
        showPrettyType(inType, context)
      } else {
        tr.toString()
      }
    }

//  val shortName = backquotedPath(imp.expr)
//  val fullName = imp.expr.symbol.fullName
//  val impName = if (shortName.length < fullName.length) fullName else shortName
  }
}