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

  def showType(compiler: nsc.interactive.Global, what: nsc.interactive.Global#Tree) = {
    val typePrinters = TypePrinters.apply(compiler)
    val contextInfo = compiler.askForResponse(
      () =>
        compiler.locateContext(what.pos)
    )
    contextInfo.get match {
      case Left(value) =>
        value match {
          case Some(context) =>
            typePrinters.showType(what, context)
          case None => what.toString()
        }
      case Right(_) => what.toString()
    }
  }
}

trait TypePrinters {
  self: PrettyPrinters =>

  val interactive: Global
  import interactive._

  def showType(what: nsc.Global#Tree, context: nsc.interactive.Global#Context) = {
    val printer = new TypePrinter()
    printer.showTypeTree(what.asInstanceOf[Tree], context.asInstanceOf[Context])
  }

  class TypePrinter {

    import interactive.shorthands
    import interactive.definitions.{isFunctionType, isTupleType, isByNameParamType, parentsString}
    import interactive.definitions.{ByNameParamClass, RepeatedParamClass}


    def showTypeTree(tr: Tree, context: Context): String = {
      val availImports = context.imports

      def showType(inType: Type): String = {
        inType match {
          case typeRef@TypeRef(pre, sym, args) => {
            def needsPreString = {
              !shorthands(sym.fullName) || (sym.ownersIterator exists (s => !s.isClass))
            }

            def preString(typeName: String, isModuleTypeRef: Boolean = false) = {
              //TODO: try to remove typeSym - we can take it from typeRef pattern matching
              //TODO: here invoke logic to find imports and ambigious imports

              System.out.println();
              availImports.foreach{
                imp =>
                  System.out.println("===> " + imp.toString() + " --- depth: " + imp.depth)
              }

              System.out.println("inType.toString: " + inType.toString())
              System.out.println("sym.name: " + sym.name)
              System.out.println("--------")

              val findImport = availImports.find{
                  _.importedSymbol(sym.name).exists
              }.getOrElse(null)

              System.out.println("foundImport: " + findImport)

//              if (findImport != null) {
//                System.out.println("findImport.qual.symbol.fullName: " + findImport.qual.symbol.fullName)
//                System.out.println("findImport.qual.tpe: " + findImport.qual.tpe)
//                System.out.println("backquotedPath(findImport.qual): " + backquotedPath(findImport.qual))
//              }

              val gettedImport = getImportForType(sym)
              System.out.println(">>>>>> gettedImport: " + gettedImport)

              val preImport = getImportForType(pre.termSymbol)
              System.out.println(">>>>>> preImport: " + preImport)

              val prePreImport = getImportForType(pre.prefix.termSymbol)
              System.out.println(">>>>>> preImport: " + prePreImport)

//              System.out.println("===== preString =====")
//              System.out.println("pre.toString: " + pre.toString())
//              System.out.println("pre.termSymbol: " + pre.termSymbol)
//              System.out.println("pre.typeSymbol: " + pre.typeSymbol)
//              System.out.println("pre.typeSymbol.name == pre.termSymbol.name: " + (pre.typeSymbol.name == pre.termSymbol.name))
//              System.out.println("pre.termSymbol.isRootPackage: " + pre.termSymbol.isRootPackage)
//              System.out.println("pre.termSymbol.isRootSymbol: " + pre.termSymbol.isRootSymbol)
//              System.out.println("pre.termSymbol.isRoot: " + pre.termSymbol.isRoot)
//              System.out.println("pre.typeSymbol: " + pre.typeSymbol)
//              System.out.println("pre.typeSymbol.isRootPackage: " + pre.typeSymbol.isRootPackage)
//              System.out.println("pre.typeSymbol.isRootSymbol: " + pre.typeSymbol.isRootSymbol)
//              System.out.println("pre.typeSymbol.isRoot: " + pre.typeSymbol.isRoot)
//              System.out.println("pre.typeSymbol.name: " + pre.typeSymbol.name)
//              System.out.println("---")

//              val findImportPackage = imports.find{
//                _.importedSymbol(pre.typeSymbol.name).exists
//              }.getOrElse(null)
//
//              System.out.println("findImportPackage: " + findImportPackage)
//              System.out.println("imports.size: " + imports.size)

//              val findImportPackage1 = availImports.find{
//                _.importedSymbol(pre.termSymbol.name).exists
//              }.getOrElse(null)
//
//              System.out.println("findImportPackage1: " + findImportPackage1) //finds by term symbol
//              System.out.println("imports.size: " + availImports.size)
//
//              if (findImportPackage1 != null) {
//                System.out.println("---------------------------")
//                System.out.println("pre.prefix: " + pre.prefix)
//                System.out.println("findImportPackage1.qual.tpe: " + findImportPackage1.qual.tpe)
////                System.out.println("pre.prefix =:= findImportPackage1.qual.tpe: " + (findImportPackage1.qual.tpe.prefix.asInstanceOf[global.Type] =:= findImportPackage1.qual.tpe.asInstanceOf[global.Type]))
////                System.out.println("pre.prefix =:= findImportPackage1.qual.tpe: " + (pre.prefix =:= findImportPackage1.qual.tpe))
//                System.out.println("---------------------------")
//              }

//              System.out.println("pre.prefixString: " + pre.prefixString)

//              if (findImportPackage1 != null) {
//                //val shortName = backquotedPath(imp.expr)
//                //val fullName = imp.expr.symbol.fullName
//                System.out.println("findImportPackage1.qual.symbol.fullName: " + findImportPackage1.qual.symbol.fullName)
//                System.out.println("backquotedPath(findImportPackage1.qual): " + backquotedPath(findImportPackage1.qual))
//              }
//
//              val pre1 = pre.prefix
//              System.out.println("pre1.toString: " + pre1.toString())
//              System.out.println("pre1.termSymbol: " + pre1.termSymbol)
//              System.out.println("pre1.termSymbol.isRootPackage: " + pre1.termSymbol.isRootPackage)
//              System.out.println("pre1.termSymbol.isRootSymbol: " + pre1.termSymbol.isRootSymbol)
//              System.out.println("pre1.termSymbol.isRoot: " + pre1.termSymbol.isRoot)
//              System.out.println("pre1.typeSymbol: " + pre1.typeSymbol)
//              System.out.println("pre1.typeSymbol.isRootPackage: " + pre1.typeSymbol.isRootPackage)
//              System.out.println("pre1.typeSymbol.isRootSymbol: " + pre1.typeSymbol.isRootSymbol)
//              System.out.println("pre1.typeSymbol.isRoot: " + pre1.typeSymbol.isRoot)
//              System.out.println("---")
//              System.out.println("pre1.prefixString: " + pre1.prefixString)

              //origPreString ends in .
              val origPreString = pre.prefixString
              //TODO check if origPreString is shorter or empty
//              System.out.println("-----> origPreString: " + origPreString)
              if (!isModuleTypeRef && needsPreString)
                if (hasImp(origPreString)) {
                  modifyPrefix(origPreString, typeName)
                } else origPreString
              else ""
            }

            //TODO get symbol based on curType and pass only type
            //type we can get - pre.tpe - for typeRef and typeRef
            def getImportForType(curSymbol: Symbol) = {
              var imports = availImports
              val name = curSymbol.name
              var ambigiousError = false
              var impSym: Symbol = NoSymbol

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
                    //TODO problems with context
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

//                  val importToPrint = imports.headOption.getOrElse(null)
//                  System.out.println("->getImportForType-imports.head: " + importToPrint)
//                  System.out.println("->  importToPrint.qual.symbol.fullName: " + importToPrint.qual.symbol.fullName)
//                  System.out.println("->  curType.prefixString: " + curType.prefixString)
//                  System.out.println("->  curSymbol.fullName: " + curSymbol.fullName)
//                  System.out.println("->getImportForType-ambigious: " + ambigiousError)
//                  System.out.println("->  curSymbol.isEmptyPrefix: " + curSymbol.isEmptyPrefix) //false
//                  System.out.println("->  curSymbol.isOmittablePrefix: " + curSymbol.isOmittablePrefix) //false
                  imports.headOption
                } else None

              resultOpt match {
                //TODO - fix and test
                case Some(impInfo) if (!ambigiousError && (curSymbol.fullName.startsWith(impInfo.qual.symbol.fullName))) => // && (impInfo.qual.tpe =:= curType.prefix)) =>
                  resultOpt
                case _ => None
              }
            }

            //TODO reimplement
            def hasImp(origPrefix: String) = false
            // (!importsMap.isEmpty && importsMap.keySet.exists(prefix => origPrefix.startsWith(prefix))) //check if there are entries in map that possible to use for import

            //TODO reimplement
            //TODO fix quoted / unquoted names
            def modifyPrefix(origPrefix: String, typeName: String): String = {
              //            def fullTypePrefix(s: ImportSelector, importPrefix: String, typeName: String, origPrefix: String) = {
              //              (origPrefix == importPrefix + ".") && (s.name.toString.trim == typeName)
              //            }
              //            //System.out.println("sorting order: " + importsMap.filterKeys(impPrefix => origPrefix.startsWith(impPrefix)).toSeq.sortWith(_._1.length > _._1.length).map{x => x._1})
              //            val importPrefix = (importsMap.filterKeys(impPrefix => origPrefix.startsWith(impPrefix)).toSeq.sortWith(_._1.length > _._1.length).find{
              //              impEntry => {
              //                val (iPrefix, impList) = impEntry
              //                impList.exists{
              //                  case isel@ImportSelector(name1, pos1, name2, pos2) =>
              //                    isWildcard(isel) || origPrefix.startsWith(iPrefix+"."+name1.toString.trim) || fullTypePrefix(isel, iPrefix, typeName, origPrefix)
              //                  case _ => false
              //                }
              //                //if (origPrefix.startsWith(importPrefix) && selector == _) or (origPrefix.equals.impPrefix && (orig.typeName == impPrefix.name)) or (origPrefix.startsWith(importPrefix + "." + selector.name))
              //              }
              //            }).getOrElse(("", List()))._1
              //            //importPrefix._1
              //
              //            if (!importPrefix.isEmpty)
              //              origPrefix.replaceFirst(importPrefix + ".", "")
              //            else
              //              origPrefix

              //1.get all imports that have prefix equal or startsWith
              //2.sort prefixes by length - 1st - equal, 2nd - startsWith_1, 3rd - startsWith_2
              //3.if equal-search imports with wildcard or import.name = type.name => remove all prefix (import.prefix
              //  if startsWith-search imports with wildcard or startsWith(import.prefix+import.name) => remove import.prefix
              origPrefix
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

          case ConstantType(t) => "showType(inType: Type):-Constant" //do we need to implement it?
          case SingleType(pre, name) => "showType(inType: Type):-SingleType"
          case annTpe @ AnnotatedType(annotations, underlying, selfsym) => "showType(inType: Type):-AnnotatedType" //annotations.mkString(underlying + " @", " @", "") - i think we don't need them in the current state
          case ext: ExistentialType => "showType(inType: Type):-ExistentialType" //??? - i think we don't need them in the current state
          case pt: PolyType => "showType(inType: Type):-PolyType" //Do we need to implement it? - it's not a TypeRef but it contains a type
          case tb: TypeBounds => "showType(inType: Type):-TypeBounds" //??? - i think we don't need them in the current state
          case _ => "showType(inType: Type):-undefined-type"
        }
      }

      //showTypeTree(tr: Tree, context: Context) implementation
      if (tr.isType && !availImports.isEmpty) {
        val inType = tr.tpe
        val result = showType(inType)
        System.out.println("showTypeTree(tr: Tree, context: Context)-RESULT: " + result)
        result
      } else {
        if (!tr.isType)
          "showTypeTree(tr: Tree, context: Context): tree_not_a_type"//TODO - change to tr.toString after testing
        else
          "showTypeTree(tr: Tree, context: Context): imports_are_empty"//TODO - change to tr.toString after testing
      }

    }

//  val shortName = backquotedPath(imp.expr)
//  val fullName = imp.expr.symbol.fullName
//  val impName = if (shortName.length < fullName.length) fullName else shortName
  }
}