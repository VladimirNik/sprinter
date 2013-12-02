package scala.sprinter.printers

import scala.tools.nsc.interactive.Global
import scala.tools.nsc
import java.io.{StringWriter, PrintWriter}

object TypePrinters {
  def apply(compiler: Global) = {
    new TypePrinters {
      val global: Global = compiler
    }
  }

  def showType(compiler: Global, what: Global#Tree, context: Global#Context) = {
    apply(compiler).showType(what, context)
  }

  def showType(compiler: Global, what: Global#Type, context: Global#Context) = {
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

trait TypePrinters extends PrettyPrinters {

  val global: scala.tools.nsc.interactive.Global
  import global._

  def showType(what: Global#Tree, context: Global#Context) = {
    val printer = new TypePrinter()
    printer.showTypeTree(what.asInstanceOf[Tree], context.asInstanceOf[Context])
  }

  def showType(what: nsc.Global#Type, context: Global#Context) = {
    val printer = new TypePrinter()
    printer.showPrettyType(what.asInstanceOf[Type], context.asInstanceOf[Context])
  }

  class TypePrinter {

    import global.{shorthands, Context}
    import global.definitions.{isFunctionType, isTupleType, isByNameParamType, parentsString}
    import global.definitions.{ByNameParamClass, RepeatedParamClass}

    import global.analyzer.ImportInfo

    def showPrettyType(inType: Type, context: Context): String = {
      lazy val availImports = context.imports
      if (!Option(context).isEmpty && !availImports.isEmpty) {

        def showType(inType: Type): String = {
          var (avImp, symSearch): (Option[ImportInfo], Symbol) = (None, NoSymbol)
          var typeRename: Option[Name] = None
          inType match {
            case typeRef@TypeRef(pre, sym, args) => {
              def needsPreString = {
                !shorthands(sym.fullName) || (sym.ownersIterator exists (s => !s.isClass))
              }

              def preString(typeName: String, isModuleTypeRef: Boolean = false) = {
                //println("================================");
                //availImports.foreach{
                //  imp =>
                //    System.out.println("===> " + imp.toString() + " --- depth: " + imp.depth)
                //}
                //println("--------")
                //println("inType.toString: " + inType.toString())
                //println("sym.name: " + sym.name)
                //println("--------")
//                println("HELLO WORLD!")

                //preString implementation
                //origPreString ends in .
                val origPreString = pre.prefixString
                //TODO check if origPreString is shorter or empty
                if (!isModuleTypeRef && needsPreString)
                  if (!origPreString.isEmpty) {
                    //impExist - if there are possible imports for type (not parts of it prefix)
                    val (impInf, fSym, impExist) = getAvailableImport()
                    avImp = impInf
                    symSearch = fSym
                    avImp match {
                      case Some(imp) => modifyPrefix(origPreString, imp)
                      case None if (!impExist) => removePackagePrefix(origPreString)
                      case None => origPreString
                    }
                  } else origPreString
                else ""
              }

              //TODO fix quoted / unquoted names
              //if result.isEmpty - use name from import
              def modifyPrefix(origPrefix: String, imInf: ImportInfo): String = {
                val qual = imInf.tree.expr.symbol.fullName

                val result =
                  if (!qual.isEmpty) {
                    importedSelector(symSearch.name, imInf) match {
                      case Some(imp @ ImportSelector(name,_, rename, _)) =>
                        val qualName = s"$qual.$name."
                        val (replaceQual, valToReplace) = if (!Option(rename).isEmpty && name != rename && origPrefix.startsWith(qualName))
                          (qualName, s"$rename.")
                        else {
                          if (!Option(rename).isEmpty && name != rename)
                            typeRename = Option(rename)
                          (s"$qual.", "")
                        }
                        origPrefix.replaceFirst(replaceQual, valToReplace)
                      case _ => origPrefix
                    }
                  } else origPrefix
                result
              }

              def removePackagePrefix(origPrefix: String): String = {
                val enclPackage = try {
                  Option(context.enclClass.tree.symbol.enclosingPackage)
                } catch {
                  case e => None
                }
                val enclPackageStr = if (enclPackage.isDefined) enclPackage.get.fullName else ""
                if (origPrefix.startsWith(enclPackageStr)) origPrefix.replaceFirst(s"$enclPackageStr.", "") else origPrefix
              }

              def getAvailableImport() = {
                val (impOpt, flag) = getImportForSymbolWithFlag(sym)
                if (impOpt.isEmpty) {
                  val (i, s) = getImportForType(pre)
                  (i, s, flag)
                } else (impOpt, sym, flag)
              }

              def getImportForType(tp: Type): (Option[ImportInfo], Symbol) = {
                val importOpt = getImportForSymbol(tp.termSymbol)
                val excList = List("<root>", "<none>", "<noprefix>")

                importOpt match {
                  case Some(imp) =>
                    (importOpt, tp.termSymbol)
                  case _ if (Option(tp).isEmpty || tp == NoType || tp.isError || !tp.isNotNull ||
                    excList.contains(tp.termSymbol.toString()) || excList.contains(tp.toString()) ||
                    tp.termSymbol == NoSymbol || tp.termSymbol.isRoot || tp.termSymbol.isRootSymbol) => (None, NoSymbol)
                  case None => getImportForType(tp.prefix)
                }
              }

              /** Is name imported explicitly, not via wildcard?
                * Here we need to check equality for original import name (not rename)
                */
              def isExplicitImport(name: Name, impInf: ImportInfo): Boolean = {
                val tree = impInf.tree
                tree.selectors exists (_.name == name.toTermName)
              }

              /** The symbol with name `name` imported from import clause `tree`.
                * Here we need to check equality for original import name (not rename)
                */
              def importedSymbol(name: Name, impInf: ImportInfo): Symbol =
                importedSymAndSelector(name, impInf)._1

              def importedSelector(name: Name, impInf: ImportInfo) =
                importedSymAndSelector(name, impInf)._2

              def importedSymAndSelector(name: Name, impInf: ImportInfo): (Symbol, Option[ImportSelector]) = {
                var (resSym, resSel): (Symbol, Option[ImportSelector]) = (NoSymbol, None)
                //var renamed = false
                val tree = impInf.tree
                val qual = impInf.qual
                var selectors = tree.selectors
                while (selectors != Nil && resSym == NoSymbol) {
                  val sel = selectors.head
                  if (sel.name == name.toTermName) {
                    resSym = qual.tpe.nonLocalMember( // new to address #2733: consider only non-local members for imports
                      if (name.isTypeName) sel.name.toTypeName else sel.name)
                  //else if (selectors.head.name == name.toTermName)
                  //   renamed = true
                    resSel = Option(sel)
                  } else if (sel.name == nme.WILDCARD) { // && !renamed)
                    resSym = qual.tpe.nonLocalMember(name)
                    resSel = Option(sel)
                  }
                  selectors = selectors.tail
                }  
                (resSym, resSel)
              }

              def getImportForSymbol(curSymbol: Symbol): Option[ImportInfo] = getImportForSymbolWithFlag(curSymbol)._1

              def getImportForSymbolWithFlag(curSymbol: Symbol): (Option[ImportInfo], Boolean) = {
                var importExists = false
                var imports = availImports
                val name = curSymbol.name
                var ambigiousError = false
                var impSym: Symbol = NoSymbol

                while (!impSym.exists && !imports.isEmpty) {
                  impSym = importedSymbol(name, imports.head)
                  if (!impSym.exists) imports = imports.tail
                }

                val resultOpt =
                  if (impSym.exists) {
                    importExists = true
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
                      (!isExplicitImport(name, imports.head) || imports1.head.depth == imports.head.depth)) {
                      impSym1 = importedSymbol(name, imports1.head)

                      if (impSym1.exists) {
                        if (isExplicitImport(name, imports1.head)) {
                          ambigiousError =
                            if (isExplicitImport(name, imports.head) || imports1.head.depth != imports.head.depth)
                              ambiguousImport()
                            else false
                          impSym = impSym1
                          imports = imports1
                        } else if (!isExplicitImport(name, imports.head) &&
                          imports1.head.depth == imports.head.depth) ambigiousError = ambiguousImport()
                      }
                      imports1 = imports1.tail
                    }

                    imports.headOption
                  } else None

                resultOpt match {
                  //TODO - fix and test (problem with =:= and synchronization)
                  case Some(impInfo) if (!ambigiousError && (curSymbol.fullName.startsWith(impInfo.qual.symbol.fullName)) && (impInfo.qual.tpe =:= curSymbol.tpe.prefix)) =>
                    (resultOpt, importExists)
                  case _ => (None, importExists)
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
                      finishPrefix(preString(typeName) +
                        (if (typeRename.isEmpty) typeName else typeRename.get.toString) +
                        argsString)
                    }
                }
              }
              //showType(inType: Type): String implementation
              //case typeRef@TypeRef(pre, sym, args) =>
              safeToString
            }

            //TODO process not TypeRef: ConstantType, SingleType, AnnotatedType, ExistentialType, PolyType, TypeBounds
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
      } else tr.toString()
    }

    //  val shortName = backquotedPath(imp.expr)
    //  val fullName = imp.expr.symbol.fullName
    //  val impName = if (shortName.length < fullName.length) fullName else shortName
  }
}