/* sprinter
 * Copyright 2007-2013 LAMP/EPFL
 */

package scala.sprinter.printers

import java.io.{StringWriter, PrintWriter}
import scala.reflect.internal.Flags._

import scala.tools.nsc
import scala.tools.nsc.ast.Printers
import nsc.Global

import scala.reflect.internal.Definitions
import scala.util.control.ControlThrowable
import scala.annotation.tailrec
import scala.reflect.internal.util.{SourceFile, Statistics}
import scala.runtime.ObjectRef
import scala.reflect.internal.util.ThreeValues._
import scala.reflect.api.Symbols
import scala.reflect.api.Types

object PrettyPrinters{
  private[PrettyPrinters] trait PrinterDescriptor
  object AFTER_NAMER extends PrinterDescriptor
  object AFTER_TYPER extends PrinterDescriptor

  def apply(global: Global) = {
    new PrettyPrinters(global)
  }
}

class PrettyPrinters(val global: Global){

  import global._

  //TODO set printMultiline for false
  def show(what: nsc.Global#Tree, printerType: PrettyPrinters.PrinterDescriptor = PrettyPrinters.AFTER_TYPER, printMultiline: Boolean = false, decodeNames: Boolean = true) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    var printer = printerType match {
      case PrettyPrinters.AFTER_NAMER => new PrettyPrinter(writer, printMultiline, decodeNames)
      case PrettyPrinters.AFTER_TYPER | _ => new AfterTyperPrinter(writer, printMultiline, decodeNames)
    }

    printer.print(what)
    writer.flush()
    buffer.toString
  }



  protected def compareNames(name1: nsc.Global#Name, name2: nsc.Global#Name) =
    !Option(name1).isEmpty && !Option(name2).isEmpty && (name1.toString.trim == name2.toString.trim)
//  {
//    if (name1 == null || name2 == null) false
//    else name1.toString.trim == name2.toString.trim
//  }

  //TODO change printMultiline for option object - to pass all parameters
  class PrettyPrinter(out: PrintWriter, printMultiline: Boolean = false, decodeNames: Boolean = true) extends global.TreePrinter(out) {
    //TODO maybe we need to pass this stack when explicitly run show inside print
    val contextStack = scala.collection.mutable.Stack[Tree]()

    private var currentOwner: Symbol = NoSymbol
    private var selectorType: Type = NoType

    override def printModifiers(tree: Tree, mods: Modifiers): Unit = printModifiers(tree, mods, false)

    def printModifiers(tree: Tree, mods: Modifiers, isCtr: Boolean): Unit =
      if (getCurrentContext().isEmpty || modsAccepted)
        printFlags(mods.flags, "" + mods.privateWithin, isCtr)
      else
        List(IMPLICIT, CASE, LAZY).foreach{flag => if(mods.hasFlag(flag))  printFlags(flag, "", isCtr)}

    def modsAccepted = getCurrentContext() map {
      case _:ClassDef | _:ModuleDef | _:Template | _:PackageDef => true
      case _ => false
    } getOrElse false

    override def printFlags(flags: Long, privateWithin: String) =
      printFlags(flags, privateWithin, false)

    def printFlags(flags: Long, privateWithin: String, isCtr: Boolean) {
      val base = PROTECTED | OVERRIDE | PRIVATE | ABSTRACT | FINAL | SEALED | LAZY | LOCAL
      val mask = if (isCtr) base else base | IMPLICIT

      val s = flagsToString(flags & mask, privateWithin)
      if (s != "") print(s + " ")
      //case flag should be the last
      val caseFlag = flagsToString(flags & CASE)
      if (!caseFlag.isEmpty) print(caseFlag + " ")
      //abs override flag should be the last
      val absOverrideFlag = flagsToString(flags & ABSOVERRIDE)
      if (!absOverrideFlag.isEmpty) print("abstract override ")
    }

    def printConstrParams(ts: List[ValDef], isConstr: Boolean) {
      codeInParantheses(){
        if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
        printSeq(ts) {
          printParam(_, true)
        } { print(", ") }
      }
    }

    override def printValueParams(ts: List[ValDef]) {
      printValueParams(ts, false)
    }

    def printValueParams(ts: List[ValDef], isFuncTree: Boolean) {
      //val a: Int => Int = implicit x => x //parantheses are not allowed here
      val printParanthesis = !isFuncTree || {
        ts match {
          case List(vd: ValDef) => !vd.mods.hasFlag(IMPLICIT)
          case _ => true
        }
      }

      if (printParanthesis)
        super.printValueParams(ts)
      else {
        if (!ts.isEmpty) printFlags(ts.head.mods.flags & IMPLICIT, "")
        printSeq(ts) {
          printParam
        } { print(", ") }
      }
    }

    def printParam(tree: Tree, isConstr: Boolean) {
      tree match {
        case ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(tree)
          if (isConstr) {
            printModifiers(tree, mods, isConstr)
          }
          print(if (mods.isMutable && isConstr) "var " else if (isConstr) "val " else "", symName(tree, name));
          if (name.endsWith("_")) print(" ");
          printOpt(": ", tp);
          printOpt(" = ", rhs)
        case _ => super.printParam(tree)
      }
    }

    override def printParam(tree: Tree) {
      printParam(tree, false)
    }

    override def printAnnotations(tree: Tree) {
      val annots = tree.asInstanceOf[MemberDef].mods.annotations
      annots foreach {
        case Apply(Select(New(tree), p), args) => val ap = Apply(tree, args)
          print("@", ap, " ")
        case ann => print("@" + ann + " ")
      }
    }

    override def printTypeParams(ts: List[TypeDef]) {
      if (!ts.isEmpty) {
        print("["); printSeq(ts){ t =>
          printAnnotations(t)
          if (t.mods.hasFlag(CONTRAVARIANT)) {
            print("-")
          } else if (t.mods.hasFlag(COVARIANT)) {
            print("+")
          }
          printParam(t)
        }{print(", ")}; print("]")
      }
    }

    def codeInParantheses(condition: Boolean = true)(body: =>Unit) {
      if (condition) print("(")
      body
      if (condition) print(")")
    }

    def specialTreeContext(context: Tree)(iIf: Boolean = true, iMatch: Boolean = true,
        iTry: Boolean = true, iAnnotated: Boolean = true, iBlock: Boolean = true, iLabelDef: Boolean = true) = {
      context match {
        case _: If => iIf
        case _: Match => iMatch
        case _: Try => iTry
        case _: Annotated => iAnnotated
        case _: Block => iBlock
        case _: LabelDef => iLabelDef
        case _ => false
      }
    }

    private def isIntLitWithDecodedOp(qual: Tree, name: Name) = {
      lazy val qualIsIntLit = qual match {
        case Literal(x) => x.value.isInstanceOf[Int]
        case _ => false
      }
      decodeNames && qualIsIntLit && name.isOperatorName
    }

    //Danger while using inheritance: it's hidden (overwritten) method
    def backquotedPath(t: Tree): String = {
      t match {
        case Select(qual, name) if (name.isTermName && specialTreeContext(qual)(iLabelDef = false)) || isIntLitWithDecodedOp(qual, name) => "(%s).%s".format(backquotedPath(qual), symName(t, name))
        case Select(qual, name) if name.isTermName  => "%s.%s".format(backquotedPath(qual), symName(t, name))
        case Select(qual, name) if name.isTypeName  => "%s#%s".format(backquotedPath(qual), symName(t, name))
        case Ident(name)                            => symName(t, name)
        case _                                      => show(t)
      }
    }

    def contextManaged(context: Tree)(body: =>Unit) {
      contextStack.push(context)
      body
      contextStack.pop()
    }

    def getCurrentContext() = if (!contextStack.isEmpty) Some(contextStack.top) else None

    def removeDefaultTypesFromList(trees: List[Tree])(classesToRemove: List[String])(traitsToRemove: List[String]) =
      removeDefaultTraitsFromList(removeDefaultClassesFromList(trees, classesToRemove), traitsToRemove)

    def removeDefaultClassesFromList(trees: List[Tree], classesToRemove: List[String]) = trees filter {
      case Select(Ident(sc), name) => !((classesToRemove.contains(name.toString)) && (sc.toString == "scala"))
      case _ => true
    }

    def removeDefaultTraitsFromList(trees: List[Tree], traitsToRemove: List[String]): List[Tree] =
      trees match {
        case Nil => trees
        case list : List[Tree] => list.last match {
          case Select(Ident(sc), name) if ((traitsToRemove.contains(name.toString)) && (sc.toString == "scala"))
            => removeDefaultTraitsFromList(list.init, traitsToRemove)
          case _ => list
         }
      }

    def getPrimaryConstr(methods: List[Tree]) =
      methods collectFirst {
        case dd: DefDef if dd.name.toString.trim == nme.CONSTRUCTOR.toString.trim => dd
      }

    // Is this selector remapping a name (i.e, {name1 => name2})
    protected def isNotRemap(s: ImportSelector): Boolean =
      (compareNames(s.name, nme.WILDCARD) || compareNames(s.name, s.rename))

    protected def selectorToString(s: ImportSelector): String = {
      val from = quotedName(s.name)
      if (isNotRemap(s)) from
      else from + "=>" + quotedName(s.rename)
    }

    override def printTree(tree: Tree) {
      tree match {
        case ClassDef(mods, name, tparams, impl) =>
          contextManaged(tree){
            printAnnotations(tree)
            val word =
              if (mods.isTrait){
                printModifiers(tree, mods &~ ABSTRACT) // avoid abstract modifier for traits
                "trait"
              } else {
                printModifiers(tree, mods)
                "class"
              }

            print(word, " ", symName(tree, name))
            printTypeParams(tparams)

            val Template(parents @ List(_*), self, methods) = impl
            if (!mods.isTrait) {
              val templateVals = methods collect {
                case ValDef(mods, name, _, _) => (name, mods)
              }

              val primaryConstrOpt = getPrimaryConstr(methods)

              primaryConstrOpt map {
                primaryConstr =>

                val cstrMods = primaryConstr.mods
                val vparamss = primaryConstr.vparamss

                //combine modifiers
                val printParamss =
                  vparamss map {
                    vparams =>
                      if (vparams.isEmpty) vparams
                      else vparams map {
                        vparam =>
                          templateVals find {
                            tv =>
                              compareNames(tv._1, vparam.name)
                          } map {
                            templateVal =>
                              ValDef(Modifiers(vparam.mods.flags | templateVal._2.flags, templateVal._2.privateWithin,
                                (vparam.mods.annotations ::: templateVal._2.annotations) distinct), vparam.name, vparam.tpt, vparam.rhs)
                          } getOrElse vparam
                      }
                    }

                //constructor's modifier
                if (cstrMods.hasFlag(AccessFlags)) {
                  print(" ")
                  printModifiers(primaryConstr, cstrMods)
                }

                //constructor's params
                printParamss foreach { printParams =>
                  //don't print single empty constructor param list
                  if (!(printParams.isEmpty && printParamss.size == 1) || cstrMods.hasFlag(AccessFlags)) {
                    printConstrParams(printParams, true)
                    print(" ")
                  }
                }
              } getOrElse {print(" ")}
            }

            //get trees without default classes and traits (when they are last)
            val printedParents = removeDefaultTypesFromList(parents)(List("AnyRef"))(if (mods.hasFlag(CASE)) List("Product", "Serializable") else Nil)

            print(if (mods.isDeferred) "<: " else if (!printedParents.isEmpty) " extends "
              else "", impl)
          }

        case PackageDef(packaged, stats) =>
          contextManaged(tree){
            packaged match {
              case Ident(name) if compareNames(name, nme.EMPTY_PACKAGE_NAME) =>
                printSeq(stats) {
                  print(_)
                } {
                  print(";");
                  println()
                };
              case _ =>
                printAnnotations(tree)
                print("package ", packaged);
                printColumn(stats, " {", ";", "}")
            }
          }

        case ModuleDef(mods, name, impl) =>
          contextManaged(tree){
            printAnnotations(tree)
            printModifiers(tree, mods);
            val Template(parents @ List(_*), self, methods) = impl
            val parentsWAnyRef = removeDefaultClassesFromList(parents, List("AnyRef"))
            print("object " + symName(tree, name), if (!parentsWAnyRef.isEmpty) " extends " else "", impl)
          }

        case vd@ValDef(mods, name, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ", symName(tree, name))
          if (name.endsWith("_")) print(" ")

          printOpt(
            // place space after symbolic def name (val *: Unit does not compile)
            (if(symName(tree, name) != symName(tree, name,false) || symName(tree, name) != symName(tree, name, true))
              " "
            else
              "") +
            ": ",
            tp
          )
          contextManaged(tree){
            if (!mods.isDeferred)
              print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd@DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print("def " + symName(tree, name))
          printTypeParams(tparams);
          vparamss foreach printValueParams
          if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty) && name.endsWith("_"))
            print(" ")
          printOpt(
            // place space after symbolic def name (def *: Unit does not compile)
            (if(symName(tree, name) != symName(tree, name,false) || symName(tree, name) != symName(tree, name, true))
              " "
            else
              "") +
            ": ",
            tp
          )
          contextManaged(tree){
            printOpt(" = " + (if (mods.hasFlag(MACRO)) "macro " else ""), rhs)
          }

        case td@TypeDef(mods, name, tparams, rhs) =>
          if (mods hasFlag (PARAM | DEFERRED)) {
            printAnnotations(tree)
            printModifiers(tree, mods);
            print("type ");
            printParam(tree)
          } else {
            printAnnotations(tree)
            printModifiers(tree, mods);
            print("type " + symName(tree, name))
            printTypeParams(tparams);
            contextManaged(tree){
              printOpt(" = ", rhs)
            }
          }

        case LabelDef(name, params, rhs) =>
          if (name.contains("while$")) {
            contextManaged(tree){
              val If(cond, thenp, elsep) = rhs
              print("while (", cond, ") ")
              val Block(list, wh) = thenp
              printColumn(list, "", ";", "")
            }
          } else if (name.contains("doWhile$")) {
            contextManaged(tree){
              val Block(bodyList: List[Tree], ifCond @ If(cond, thenp, elsep)) = rhs
              print("do ")
              printColumn(bodyList, "", ";", "")
              print(" while (", cond, ") ")
            }
          } else {
            print(symName(tree, name)); printLabelParams(params);
            contextManaged(tree){
              printBlock(rhs)
            }
          }

        case Import(expr, selectors) =>
          print("import ", backquotedPath(expr), ".")
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) print(selectorToString(s))
              else print("{", selectorToString(s), "}")
            // If there is more than one selector braces are always needed
            case many =>
              print(many.map(selectorToString).mkString("{", ", ", "}"))
          }

        case Template(parents, self, body) =>
          val currentOwner1 = currentOwner
          //TODO repair using of currentOwner
          //if (tree.symbol != NoSymbol) currentOwner = tree.symbol.owner

          val printedParents =
            getCurrentContext() map {
              //val example: Option[AnyRef => Product1[Any] with AnyRef] = ... - CompoundTypeTree with template
              case _: CompoundTypeTree => parents
              case ClassDef(mods, name, _, _) if mods.hasFlag(CASE) => removeDefaultTypesFromList(parents)(List("AnyRef"))(List("Product", "Serializable"))
              case _ => removeDefaultClassesFromList(parents, List("AnyRef"))
            } getOrElse(parents)

          val primaryCtrOpt = getPrimaryConstr(body)
          var ap: Option[Apply] = None

          for (primaryCtr <- primaryCtrOpt) {
            primaryCtr match {
              case DefDef(_, _, _, _, _, Block(ctBody @ List(_*), _)) =>
                ap = ctBody collectFirst {
                  case apply: Apply => apply
                }

                //vals in preinit blocks
                val presuperVals = ctBody filter {
                  case vd:ValDef => vd.mods.hasFlag(PRESUPER)
                  case _ => false
                }

                if (!presuperVals.isEmpty) {
                  print("{")
                  printColumn(presuperVals, "", ";", "")
                  print("} " + (if (!printedParents.isEmpty) "with " else ""))
                }

              case _ =>
            }
          }

          if (!printedParents.isEmpty) {
            val (clParent :: traits) = printedParents
            print(clParent)

            def getConstrParams(tree: Tree, cargs: List[List[Tree]]): List[List[Tree]] = {
              tree match {
                case Apply(inTree, args) =>
                  getConstrParams(inTree, cargs):+args
                case _ => cargs
              }
            }

            val applyParamsList = ap map {getConstrParams(_, Nil)} getOrElse Nil
            applyParamsList foreach {x: List[Tree] => if (!(x.isEmpty && applyParamsList.size == 1)) printRow(x, "(", ", ", ")")}

            if (!traits.isEmpty) {
              printRow(traits, " with ", " with ", "")
            }
          }
          //remove primary constr def and constr val and var defs
          //right contains all constructors
          //TODO see impl filter on Tree
          val (left, right) = body.filter {
            //remove valdefs defined in constructor and pre-init block
            case vd: ValDef => !vd.mods.hasFlag(PARAMACCESSOR) && !vd.mods.hasFlag(PRESUPER)
            case dd: DefDef => !compareNames(dd.name, nme.MIXIN_CONSTRUCTOR) //remove $this$ from traits
            case EmptyTree => false
            case _ => true
          } span {
            case dd: DefDef => !compareNames(dd.name, nme.CONSTRUCTOR)
            case _ => true
          }

          val modBody = left ::: right.drop(1)//List().drop(1) ==> List()
          val showBody = !(modBody.isEmpty &&
            (self match {
              case ValDef(mods, name, TypeTree(), rhs) if (mods & PRIVATE) != 0 && name.decoded == "_" && rhs.isEmpty => true // workaround for superfluous ValDef when parsing class without body using quasi quotes
              case _ => self.isEmpty
            }))
          if (showBody) {
            if (!compareNames(self.name, nme.WILDCARD)) {
              print(" { ", self.name);
              printOpt(": ", self.tpt);
              print(" =>")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : ", self.tpt, " =>")
            } else {
              print(" {")
            }
            contextManaged(tree) {
              printColumn(modBody, "", ";", "}")
            }
          }
          currentOwner = currentOwner1

        case Block(stats, expr) =>
          contextManaged(tree){
            printColumn(stats ::: List(expr), "{", ";", "}")
          }

        case Match(selector, cases) =>
          //insert braces if match is inner
          //make this function available for other casses
          //passing required type for checking
          def insertBraces(body: =>Unit) {
            if (contextStack.exists{
              _.isInstanceOf[Match]
            }) {
                print("(")
                body
                print(")")
            } else body
          }

          val selectorType1 = selectorType
          selectorType = selector.tpe

          val printParantheses = specialTreeContext(selector)(iLabelDef = false)
          tree match {
            case Match(EmptyTree, cs) =>
              printColumn(cases, "{", "", "}")
            case _ =>
              insertBraces {
                contextManaged(tree){
                  codeInParantheses(printParantheses) {
                    print(selector);
                  }
                }
                printColumn(cases, " match {", "", "}")
              }
          }
          selectorType = selectorType1

        case CaseDef(pat, guard, body) =>
          print("case ")
          def patConstr(pat: Tree): Tree = pat match {
            case Apply(fn, args) => patConstr(fn)
            case _ => pat
          }
          if (showOuterTests &&
            needsOuterTest(
              patConstr(pat).tpe.finalResultType, selectorType, currentOwner))
            print("???")
          print(pat);
          printOpt(" if ", guard)
          contextManaged(tree) {
            print(" => ", body)
          }

        case Star(elem) =>
          print(elem, "*")

        case Bind(name, t) =>
          if (t == EmptyTree) print("(", symName(tree, name), ")")
          else if (t.exists{
            case _:Star => true
            case _ => false
          }) print(symName(tree, name), " @ ", t)
          else print("(", symName(tree, name), " @ ", t, ")")

        //almost the same as in original
        case Function(vparams, body) =>
          print("(");
          printValueParams(vparams, true);
          print(" => ", body, ")")

        case Typed(expr, tp) =>
          tp match {
            case Function(List(), EmptyTree) => print("(", expr, " _)") //func _
            case _ => print("((", expr, "): ", tp, ")") //parenteses required when (a match {}) : Type
          }

        case Apply(fun, vargs) =>
          //process methods ending on colon with multiple args list//
          //example:
//          def t[A,B](as: List[A]) = {
//            println("hello")
//            ((Map.empty[B, List[A]]) /: as){ (nels, a) => println(""); (nels)}
//          }
//         by default results in:
//        {
//          val x$1 = Map.empty[B, List[A]];
//          as.$div$colon(x$1)
//        }(((nels, a) => {
//          println("");
//          nels
//        }))
          tree match {
            //processing methods ending on colons (x \: list)
            case Apply(Block(l1 @ List(sVD :ValDef), a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))), l3 @ List(_*))
              if sVD.mods.hasFlag(SYNTHETIC) && methodName.toString.endsWith("$colon") && compareNames(sVD.name, iVDName) => //&& (sVD.name.toString.trim == iVDName.toString.trim) =>
              val printBlock = Block(l1, Apply(a1, l3))
              print(printBlock)
            case Apply(tree1, _) if (specialTreeContext(tree1)(iAnnotated = false)) => codeInParantheses(){print(fun)}; printRow(vargs, "(", ", ", ")")
            case _ => print(fun); printRow(vargs, "(", ", ", ")")
          }


        case Super(This(qual), mix) =>
          if (!qual.isEmpty || tree.symbol != NoSymbol) print(symName(tree, qual) + ".")
          print("super")
          if (!mix.isEmpty)
            print("[" + mix + "]")

        case This(qual) =>
          //symName is redefined
          if (!qual.isEmpty) print(symName(tree, qual) + ".")
          print("this")

        //case Select(apply: Apply, name) if (!settings.debug.value) =>
        //print(apply,".",symName(tree, name))

        case Select(qual@New(tpe), name) =>
          print(qual)

        case Select(qualifier, name) => {
          val printParantheses = specialTreeContext(qualifier)(iAnnotated = false) || isIntLitWithDecodedOp(qualifier, name)
          if (printParantheses) print("(", backquotedPath(qualifier), ").", symName(tree, name))
          else print(backquotedPath(qualifier), ".", symName(tree, name))
        }

        case id@Ident(name) =>
          if (!name.isEmpty) {
            val str = symName(tree, name)

            val strIsBackquoted = str.startsWith("`") && str.endsWith("`")

            print(if (id.isBackquoted && !strIsBackquoted) "`" + str + "`" else str)
          }
          else {
            print("")
          }

        case l@Literal(x) =>
          //TODO refactor multiline string processing - x.stringValue
          if (x.value.isInstanceOf[String] && printMultiline && x.stringValue.contains("\n") && !x.stringValue.contains("\"\"\"") && x.stringValue.size > 1) {
            val splitValue = x.stringValue.split('\n'.toString).toList
            val multilineStringValue = if (x.stringValue.endsWith("\n")) splitValue :+ "" else splitValue
            val trQuotes = "\"\"\""
            print(trQuotes); printSeq(multilineStringValue){print(_)}{print("\n")}; print(trQuotes)
          } else {
            //processing Float constants
            val printValue = x.escapedStringValue + (if (x.value.isInstanceOf[Float]) "F" else "") //correct printing of Float
            print(printValue)
          }

        case Annotated(Apply(Select(New(tpt), nme.CONSTRUCTOR), args), tree) =>
          def printAnnot() {
            print("@", tpt)
            if (!args.isEmpty)
              printRow(args, "(", ",", ")")
          }

          val printParantheses = specialTreeContext(tree)()
          codeInParantheses(printParantheses){print(tree)}; print(if (tree.isType) " " else ": ")
          printAnnot()

        case SelectFromTypeTree(qualifier, selector) =>
          print("(", qualifier, ")#", symName(tree, selector))

        case CompoundTypeTree(templ) =>
          contextManaged(tree){
            print(templ)
          }

        case AppliedTypeTree(tp, args) =>
          //it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
          def containsByNameTypeParam =
            args exists {
                case AppliedTypeTree(Select(qual, name), _) => name.toString.trim.equals("<byname>")
                case _ => false
              }

          if (containsByNameTypeParam) {
            print("(")
            printRow(args.init, "(", ", ", ")")
            print(" => ", args.last, ")")
          } else {
            if (tp.exists {
              case Select(_, name) => compareNames(name, tpnme.REPEATED_PARAM_CLASS_NAME)
              case _ => false
            } && !args.isEmpty) {
              print(args(0), "*")
            } else if (tp match {
              case Select(_, name) => compareNames(name, tpnme.BYNAME_PARAM_CLASS_NAME)
              case _ => false
            }) {
              print("=> ", if (args.isEmpty) "()" else args(0))
            } else {
              print(tp);
              printRow(args, "[", ", ", "]")
            }
          }

        case ExistentialTypeTree(tpt, whereClauses) =>
          print("(", tpt);
          printColumn(whereClauses, " forSome { ", ";", "})")

        case tbt@TypeBoundsTree(lo, hi) => {
          val loDefault = "_root_.scala.Nothing"
          val hiDefault = "_root_.scala.Any"
          if (loDefault != lo.toString()) printOpt(" >: ", lo); if (hiDefault != hi.toString()) printOpt(" <: ", hi)
        }

        case emptyTree if emptyTree.toString == "<empty>" => // workaround as case EmptyTree does not work for all universes because of path depedent types

        case tree => super.printTree(tree)
      }
      if (printTypes && tree.isTerm && !tree.isEmpty) {
        print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    //Danger: it's overwritten method - can be problems with inheritance)
    def symName(tree: Tree, name: Name, decoded: Boolean = decodeNames): String = {
      val encName = name.encoded
      val decName = name.decoded
      def modifyEncoded(s: String) = if (decoded && (encName.contains("$u") ||
        (encName.contains("$") && decName.exists(ch => opSym.contains(ch)) && decName.exists(ch => !opSym.contains(ch)) && !excList.exists(str => decName.contains(str)))))
        "`%s`" format s else s

      if (compareNames(name, nme.CONSTRUCTOR)) "this"
      else modifyEncoded(quotedName(name, decoded))
    }

    val opSym = List('~', '=', '<', '>', '!', '#', '%', '^', '&', '|', '*', '/', '+', '-', ':', '\\', '?', '@')
    val excList = List("\\", "_*")

    override def print(args: Any*): Unit = {
      args foreach {
        arg =>
        //TODO repair issue with pattern matching, trees and vals of type Any
          if (arg.isInstanceOf[Tree]) { //problem with vars of type Any
          val treeArg = arg.asInstanceOf[Tree]
            printTree(treeArg)
          } else {
            arg match {
              case name: Name =>
                print(quotedName(name))
              case other => super.print(other)
            }
          }
      }
    }
  }

  //TODO change printMultiline for option object - to pass all parameters
  class AfterTyperPrinter(out: PrintWriter, printMultiline: Boolean = false, decodeNames: Boolean = true) extends PrettyPrinter(out, printMultiline, decodeNames)
}
