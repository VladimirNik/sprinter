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

  class TypePrinter {//(out: PrintWriter) extends AfterTyperPrinter(out) { //TODO shouldn't extend AfterTyperPrinter, remove after testing

    import interactive.shorthands
    import interactive.definitions.{isFunctionType, isTupleType, isByNameParamType, parentsString}
    import interactive.definitions.{ByNameParamClass, RepeatedParamClass}

    private def showType(inType: Type): String = {
      inType match {
        case typeRef@TypeRef(pre, sym, args) => {
          //System.out.println("pre.prefixString: " + pre.prefixString)

          def needsPreString = {
            !shorthands(sym.fullName) || (sym.ownersIterator exists (s => !s.isClass))
          }

          def preString(typeName: String, isModuleTypeRef: Boolean = false) = {
            //origPreString ends in .
            val origPreString = pre.prefixString
            if (!isModuleTypeRef && needsPreString)
              if (hasImp(origPreString)) {
                modifyPrefix(origPreString, typeName)
              } else origPreString
            else ""
          }

          //TODO reimplement
          def hasImp(origPrefix: String) = false
//            (!importsMap.isEmpty && importsMap.keySet.exists(prefix => origPrefix.startsWith(prefix))) //check if there are entries in map that possible to use for import

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
            System.out.println("***** safeToString *****")
            //System.out.println("inType.toString: " + inType.toString())

            typeRef match {
              case mtr: ModuleTypeRef =>
                val tName = sym.nameString
                if (sym.isOmittablePrefix) "" else if (tName.isEmpty) typeRef.toString() else preString(tName, true) + tName + "." + "type"
              case _ => //other TypeRef
                val custom = customToString
                //System.out.println(">>> custom: " + custom)
                if (custom != "") custom
                else {
                  val typeName = sym.nameString
                  //System.out.println("!!! preString: " + preString(typeName))
                  finishPrefix(preString(typeName) + typeName + argsString)
                }
            }
          }

          safeToString
        }

        case ConstantType(t) => "not-implemented(Constant)" //do we need to implement it?
        case SingleType(pre, name) => "not-implemented(SingleType)"
        case annTpe @ AnnotatedType(annotations, underlying, selfsym) => "not-implemented(AnnotatedType)" //annotations.mkString(underlying + " @", " @", "") - i think we don't need them in the current state
        case ext: ExistentialType => "not-implemented(ExistentialType)" //??? - i think we don't need them in the current state
        case pt: PolyType => "not-implemented(PolyType)" //Do we need to implement it? - it's not a TypeRef but it contains a type
        case tb: TypeBounds => "not-implemented(TypeBounds)" //??? - i think we don't need them in the current state
        case _ => "not-implemented(undefined-type)"
      }
    }

    def showTypeTree(tr: Tree, context: Context): String = {
      val imports = context.imports

      if (tr.isType && !imports.isEmpty) {
        //TODO set checking for empty imports and correct tree/type
//        initImportsMap(imports)
        val inType = tr.tpe
        val result = showType(inType)
        System.out.println("result (showTypeTree(tr: Tree, imports: List[Import])): " + result)
        result
      } else {
        if (!tr.isType)
          "passed_tr_is_not_tree"//TODO - change to tr.toString after testing
        else if (imports.isEmpty)
          "imports_are_empty"
        else "problem_with_showTypeTree"
      }
    }

//    //TODO change to lazy val
//    private var imports: List[Import] = List() //TODO - maybe we don't need this list and can remove it
//    private var importsMap: scala.collection.mutable.Map[String, List[ImportSelector]] = scala.collection.mutable.Map.empty

//    def initImportsMap(imports: List[Import]) = {
//      this.imports = imports
//      imports map {
//        imp => {
//          imp match {
//            case Import(exp, selectors) if selectors.exists(im =>
//              im.name.toString == nme.WILDCARD.toString
//            ) =>
//              val all = exp.tpe.members.filter(_.isPackage).map(_.name).toSet
//              System.out.println(">>>>>>> all: " + all)
//            case _ =>
//          }
//          val shortName = backquotedPath(imp.expr)
//          val fullName = imp.expr.symbol.fullName
//          System.out.println("imp.symbol.children: " + imp.symbol.children)
//          System.out.println("imp.symbol.ancestors: " + imp.symbol.ancestors)
//          System.out.println("shortName (in imports) = " + shortName)
//          System.out.println("fullName (in imports) = " + fullName)
//          val impName = if (shortName.length < fullName.length) fullName else shortName
//
//          //get value or none
//          importsMap(impName) = (importsMap get impName) match {
//            case Some(list) =>
//              //TODO - maybe add _ (wildcard) checking (if we have _ - we don't need to add another imports)
//              list ::: imp.selectors distinct
//            case None => imp.selectors
//          }
//        }
//      }
//    }
  }
}
