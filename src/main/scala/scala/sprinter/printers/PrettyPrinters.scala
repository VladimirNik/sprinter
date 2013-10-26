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
import scala.reflect.internal.util.Statistics
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

class PrettyPrinters(val global: Global) {

  import global._

  def show(what: nsc.Global#Tree, printerType: PrettyPrinters.PrinterDescriptor = PrettyPrinters.AFTER_TYPER) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    var printer = printerType match {
      case PrettyPrinters.AFTER_NAMER => new PrettyPrinter(writer)
      case PrettyPrinters.AFTER_TYPER | _ => new AfterTyperPrinter(writer)
    }

    printer.print(what)
    writer.flush()
    buffer.toString
  }

  //TODO remove - only for test purposes
  def showType(what: nsc.Global#Tree) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    var printer = new TypePrinter(writer)

    printer.print(what)
    writer.flush()
    buffer.toString
  }

  def showType(what: nsc.Global#Tree, imports: List[nsc.Global#Import]) = {
    val buffer = new StringWriter()
    val writer = new PrintWriter(buffer)

    var printer = new TypePrinter(writer)

    printer.showTypeTree(what.asInstanceOf[global.Tree], imports.asInstanceOf[List[global.Import]])
    writer.flush()
    buffer.toString
  }

  class PrettyPrinter(out: PrintWriter) extends global.TreePrinter(out) {
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

    def compareNames(name1: Name, name2: Name) = {
      if (name1 == null || name2 == null) false
      else name1.toString.trim == name2.toString.trim
    }

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

    //Danger while using inheritance: it's hidden (overwritten) method
    def backquotedPath(t: Tree): String = {
      t match {
        case Select(qual, name) if (name.isTermName && specialTreeContext(qual)(iLabelDef = false)) => "(%s).%s".format(backquotedPath(qual), symName(t, name))
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

    override def printTree(tree: Tree) {
      tree match {
        case EmptyTree =>

        case ClassDef(mods, name, tparams, impl) =>
          contextManaged(tree){
            printAnnotations(tree)
            printModifiers(tree, mods)
            val word =
              if (mods.isTrait) "trait"
              else "class"

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
                            _._1.toString.trim == vparam.name.toString.trim
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
                } else print(" ")

                //constructor's params
                printParamss foreach { printParams =>
                  //don't print single empty constructor param list
                  if (!(printParams.isEmpty && printParamss.size == 1) || cstrMods.hasFlag(AccessFlags)) {
                    printConstrParams(printParams, true)
                    print(" ")
                  }
                }
              } getOrElse {print(" ")}
            } else print(" ")

            //get trees without default classes and traits (when they are last)
            val printedParents = removeDefaultTypesFromList(parents)(List("AnyRef"))(if (mods.hasFlag(CASE)) List("Product", "Serializable") else Nil)

            print(if (mods.isDeferred) "<: " else if (!printedParents.isEmpty) "extends "
              else "", impl)
          }

        case PackageDef(packaged, stats) =>
          contextManaged(tree){
            packaged match {
              case Ident(name) if name.toString.trim == nme.EMPTY_PACKAGE_NAME.toString.trim =>
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
            print("object " + symName(tree, name), if (!parentsWAnyRef.isEmpty) " extends " else " ", impl)
          }

        case vd@ValDef(mods, name, tp, rhs) =>
          printAnnotations(tree)
          printModifiers(tree, mods)
          print(if (mods.isMutable) "var " else "val ", symName(tree, name))
          if (name.endsWith("_")) print(" "); printOpt(": ", tp)
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
          printOpt(": ", tp);
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
          // Is this selector remapping a name (i.e, {name1 => name2})
          def isNotRemap(s: ImportSelector): Boolean = {
            (compareNames(s.name, nme.WILDCARD) || compareNames(s.name, s.rename))
          }
          def selectorToString(s: ImportSelector): String = {
            val from = quotedName(s.name)
            if (isNotRemap(s)) from
            else from + "=>" + quotedName(s.rename)
          }
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
            case dd: DefDef => dd.name.toString.trim != nme.MIXIN_CONSTRUCTOR.toString.trim //remove $this$ from traits
            case EmptyTree => false
            case _ => true
          } span {
            case dd: DefDef => dd.name.toString.trim != nme.CONSTRUCTOR.toString.trim
            case _ => true
          }

          val modBody = left ::: right.drop(1)//List().drop(1) ==> List()
          if (!modBody.isEmpty || !self.isEmpty) {
            if (self.name.toString.trim != nme.WILDCARD.toString.trim) {
              print(" { ", self.name);
              printOpt(": ", self.tpt);
              print(" => ")
            } else if (!self.tpt.isEmpty) {
              print(" { _ : ", self.tpt, " => ")
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
              if sVD.mods.hasFlag(SYNTHETIC) && methodName.toString.endsWith("$colon") && (sVD.name.toString.trim == iVDName.toString.trim) =>
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
          val printParantheses = specialTreeContext(qualifier)(iAnnotated = false)
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

        case Literal(x) =>
          //processing Float constants
          val printValue = x.escapedStringValue + (if (x.value.isInstanceOf[Float]) "F" else "") //correct printing of Float
          print(printValue)

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
              case Select(_, name) => name.toString.trim == tpnme.REPEATED_PARAM_CLASS_NAME.toString.trim
              case _ => false
            } && !args.isEmpty) {
              print(args(0), "*")
            } else if (tp match {
              case Select(_, name) => name.toString.trim == tpnme.BYNAME_PARAM_CLASS_NAME.toString.trim
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

        case tree => super.printTree(tree)
      }
      if (printTypes && tree.isTerm && !tree.isEmpty) {
        print("{", if (tree.tpe eq null) "<null>" else tree.tpe.toString, "}")
      }
    }

    //Danger: it's overwritten method - can be problems with inheritance)
    def symName(tree: Tree, name: Name, decoded: Boolean = false): String =
      if (name == nme.CONSTRUCTOR) "this"
      else quotedName(name, decoded)

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

  class AfterTyperPrinter(out: PrintWriter) extends PrettyPrinter(out)

  class TypePrinter(out: PrintWriter) extends AfterTyperPrinter(out) {

    val defs = global.asInstanceOf[Definitions];
    import defs.definitions._

//    def printTypeTree(tpe: Tree) = {
//      tpe.toString()
//    }

    var testImportsList: List[Import] = null;

    override def printTree(tree: Tree) {
      tree match {
        case p:PackageDef =>
          testImportsList = (tree.filter{
            case i:Import => true
            case _ => false
          }).asInstanceOf[List[Import]]
          super.printTree(tree)
        case vd@ValDef(mods, name, tp, rhs) =>
          System.out.println("---------------- show valdef -----------------");
          System.out.println("name: " + name)
          System.out.println("showRaw valdef: " + showRaw(vd))
          System.out.println("imports size: " + testImportsList.size)
          System.out.println(">>>>>>>>>>> printTree: " + showTypeTree(tp, testImportsList))
          System.out.println("----------------------------------------------")
          super.printTree(vd)
        case imp@Import(expr, selectors) =>

          expr match {
            case Ident(name) => System.out.println("Name of ident: " + name.toString)
            case _ =>
          }
          System.out.println("--- show import ---");
          System.out.println("showRaw import: " + showRaw(imp))
          System.out.println("import expr: " + backquotedPath(expr))
          selectors match {
            case List(s) =>
              // If there is just one selector and it is not remapping a name, no braces are needed
              if (isNotRemap(s)) System.out.println("selector: " + selectorToString(s))
              else System.out.println("selector: " + selectorToString(s))
            // If there is more than one selector braces are always needed
            case many =>
              System.out.println("selector: " + many.map(selectorToString).mkString("{", ", ", "}"))
          }
          super.printTree(imp)
        case _ => super.printTree(tree)
      }
    }

    //TODO - refactor - use shorthands from import
    val shorthands = Set(
      "scala.collection.immutable.List",
      "scala.collection.immutable.Nil",
      "scala.collection.Seq",
      "scala.collection.Traversable",
      "scala.collection.Iterable",
      "scala.collection.mutable.StringBuilder",
      "scala.collection.IndexedSeq",
      "scala.collection.Iterator")

    val symbols = global.asInstanceOf[Symbols]
    val types = global.asInstanceOf[Types]
    val typesInt = global.asInstanceOf[scala.reflect.internal.Types]
    val symbolTable = global.asInstanceOf[scala.tools.nsc.symtab.SymbolTable]

    def showType(inType: Type): String = {
      inType match {

        case TypeRef(pre, sym, args) => {
          System.out.println("pre.prefixString: " + pre.prefixString)
          System.out.println("pre: " + pre)

          def needsPreString = {
            !shorthands(sym.fullName) || (sym.ownersIterator exists (s => !s.isClass))
          }

          def preString(typeName: String) = {
            //origPreString ends in .
            val origPreString = pre.prefixString
            if (needsPreString)
              if (hasImp(origPreString)) {
                modifyPrefix(origPreString, typeName)
              } else origPreString
            else ""
          }

          def hasImp(origPrefix: String) =
            (!importsMap.isEmpty && importsMap.keySet.exists(prefix => origPrefix.startsWith(prefix))) //check if there are entries in map that possible to use for import

          //TODO fix quoted / unquoted names
          def modifyPrefix(origPrefix: String, typeName: String): String = {
            def fullTypePrefix(s: ImportSelector, importPrefix: String, typeName: String, origPrefix: String) = {
              (origPrefix == importPrefix + ".") && (s.name.toString.trim == typeName)
            }
            System.out.println("sorting order: " + importsMap.filterKeys(impPrefix => origPrefix.startsWith(impPrefix)).toSeq.sortWith(_._1.length > _._1.length).map{x => x._1})
            val importPrefix = (importsMap.filterKeys(impPrefix => origPrefix.startsWith(impPrefix)).toSeq.sortWith(_._1.length > _._1.length).find{
              impEntry => {
                val (iPrefix, impList) = impEntry
                impList.exists{
                  case isel@ImportSelector(name1, pos1, name2, pos2) =>
                    isWildcard(isel) || origPrefix.startsWith(iPrefix+"."+name1.toString.trim) || fullTypePrefix(isel, iPrefix, typeName, origPrefix)
                  case _ => false
                }
                //if (origPrefix.startsWith(importPrefix) && selector == _) or (origPrefix.equals.impPrefix && (orig.typeName == impPrefix.name)) or (origPrefix.startsWith(importPrefix + "." + selector.name))
              }
            }).getOrElse(("", List()))._1
            //importPrefix._1

            if (!importPrefix.isEmpty)
              origPrefix.replaceFirst(importPrefix + ".", "")
            else
              origPrefix

            //1.get all imports that have prefix equal or startsWith
            //2.sort prefixes by length - 1st - equal, 2nd - startsWith_1, 3rd - startsWith_2
            //3.if equal-search imports with wildcard or import.name = type.name => remove all prefix (import.prefix
            //  if startsWith-search imports with wildcard or startsWith(import.prefix+import.name) => remove import.prefix
          }

          def argsString = if (args.isEmpty) "" else args.map(tp => showType(tp)).mkString("[", ",", "]")
          def finishPrefix(rest: String) =
            inType match {
              case rtr: RefinementTypeRef => "" + rtr.thisInfo //TODO - maybe we need to add customToString(rtr.thisInfo)
              case ptr: PackageTypeRef => "package " + rest //TODO - it's default implementation
              case mtr: ModuleTypeRef => "object " + rest //TODO - it's default implementation
              case tr@ TypeRef(pre, sym, args) =>
                if (sym.isInitialized && sym.isAnonymousClass && !phase.erasedTypes)
                  //inType.toString
                  symbolTable.definitions.parentsString(sym.asInstanceOf[symbolTable.Symbol].info.parents) + tr.refinementString
                else rest
              case _ => inType.toString()
            }


          def customToString(inType: Type) = inType.typeSymbol.asInstanceOf[symbols.Symbol] match {
            case defs.definitions.RepeatedParamClass => showType(args.head) + "*"
            case defs.definitions.ByNameParamClass   => "=> " + showType(args.head)
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

          //TODO - refactor just to import
          def isTupleType(in: Type) = symbolTable.definitions.isTupleType(in.asInstanceOf[symbolTable.Type])
          def isFunctionType(in: Type) = symbolTable.definitions.isFunctionType(in.asInstanceOf[symbolTable.Type])
          def isByNameParamType(in: Type) = symbolTable.definitions.isByNameParamType(in.asInstanceOf[symbolTable.Type])

          def safeToString(inType: Type) = {
            System.out.println("***** safeToString *****")
            System.out.println("(1): inType.toString: " + inType.toString())
            inType match {
              case mtr: ModuleTypeRef =>
                if (sym.isOmittablePrefix) "" else pre.prefixString + sym.nameString + "." + "type"
              case tr: TypeRef =>
                val TypeRef(_, tsym, _) = inType
                val custom = customToString(tr)
                System.out.println(">>> custom: " + custom)
                if (custom != "") custom

                else {
                  val typeName = tsym.nameString
                  System.out.println("!!! preString: " + preString(typeName))
                  finishPrefix(preString(typeName) + typeName + argsString)
                }
              case _ => inType.toString()
            }

          }

          safeToString(inType)
        }

        case ConstantType(t) => "not-implemented(Constant)"
        case SingleType(pre, name) => "not-implemented(SingleType)"
        case annTpe @ AnnotatedType(annotations, underlying, selfsym) => "not-implemented(AnnotatedType)"
        case _ => System.out.println("Type is not found"); "not-implemented(undefined-type)"
      }
    }

    def showTypeTree(tr: Tree, imports: List[Import]): String = {
      System.out.println("tr.isInstanceOf[TypTree]: " + tr.isInstanceOf[TypTree])  //false
      System.out.println("tr.isType: " + tr.isType)  //true
      System.out.println("tr.tpe: " + tr.tpe)  //null
//      if (tr.isInstanceOf[TypTree]) {

      //TODO set checking for empty imports and correct tree
      initImportsMap(imports)

      if (tr.isType) {
        val inType = tr.tpe
        showType(inType)
      } else
        "something wrong"//tr.toString
    }

    //TODO reuse this methods
    //TODO fix Remap
    def isNotRemap(s: ImportSelector): Boolean = (compareNames(s.name, nme.WILDCARD) || compareNames(s.name, s.rename))
    def selectorToString(s: ImportSelector): String = {
      val from = quotedName(s.name)
      if (isNotRemap(s)) from
      else from + "=>" + quotedName(s.rename)
    }
    def isWildcard(s: ImportSelector): Boolean = {
      compareNames(s.name, nme.WILDCARD)
    }


    //TODO change to lazy val and remove null
    private var imports: List[Import] = List()
    private var importsMap: scala.collection.mutable.Map[String, List[ImportSelector]] = scala.collection.mutable.Map.empty

    def initImportsMap(imports: List[Import]) = imports map {
      this.imports = imports
      imp => {
        val impName = backquotedPath(imp.expr)
        //get value or none
        importsMap(impName) = (importsMap get impName) match {
          case Some(list) =>
            //if for some package we have _ than all possible types and packages are imported
            //if (list.exists(isWildcard _))
            //  list
            //else
              list ::: imp.selectors distinct
          case None => imp.selectors
        }
      }
    }
  }
}
