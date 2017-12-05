package tir

import scala.collection.mutable.Set
import toplev.GenericPrintable
import tpass.TPass

/* That is, we lose some type saftey for the sake
 * of flexibility (an intuitivity). Any TExp
 * may be replaced by any other TExp.
 */

sealed trait TExp extends TTree {
  def nodeClone: TExp
}

case class TExpConst(var const: TConst) extends TExp {
  def prettyPrint = const.prettyPrint

  def nodeClone = new TExpConst(const.nodeClone)
}

case class TExpIdent(var ident: TIdent) extends TExp {
  def prettyPrint = ident.prettyPrint

  def nodeClone = new TExpIdent(ident.nodeClone)
}

case class TExpFunApp(var funname: TExp, var application: TExp,
                      var callType: TInternalIdentVar) extends TExp {
  def prettyPrint =
    "(" + funname.prettyPrint + ") (" + application.prettyPrint + ")"

  def nodeClone =
    new TExpFunApp(funname.nodeClone, application.nodeClone, callType.nodeClone)
}

case class TExpTuple(var elems: List[TExp]) extends TExp {
  def prettyPrint = "(" + elems.map(_.prettyPrint).mkString(", ") + ")"

  def nodeClone =
    new TExpTuple(elems.map(_.nodeClone))
}

case class TExpList(var elems: List[TExp]) extends TExp {
  def prettyPrint = "[" + elems.map(_.prettyPrint).mkString(", ") + "]"

  def nodeClone =
    new TExpList(elems.map(_.nodeClone))
}

case class TExpSeq(var seq: List[TExp]) extends TExp {
  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(";\n") + ")"

  def nodeClone =
    new TExpSeq(seq.map(_.nodeClone))
}

case class TExpLetIn(var decs: List[TDec], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def prettyPrint =
  """
  |let
  |%s
  |in
  |%s
  |end
  """.stripMargin.format(decs.map(_.prettyPrint).mkString("\n"),
                         exp.prettyPrint)

  // Note that we do not nodeClone the type environment.
  def nodeClone =
    new TExpLetIn(decs.map(_.nodeClone), exp.nodeClone, env)
}

// Note that application type is a function type here, with
// type from exp -> cases.
case class TExpCase(var exp: TExp, var cases: List[TExpMatchRow],
                    var applicationType: TInternalIdentVar)
    extends TExp {
  def prettyPrint = """
  |match %s with
  |   %s
  """.stripMargin.format(exp.prettyPrint,
                         cases.map(_.prettyPrint).mkString("\n   |"))

  def nodeClone =
    new TExpCase(exp.nodeClone, cases.map(_.nodeClone), applicationType.nodeClone)
}

case class TExpMatchRow(var pat: List[TPat], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def prettyPrint =
    pat.map(_.prettyPrint).mkString(" ") + " => " + exp.prettyPrint

  def nodeClone =
    new TExpMatchRow(pat.map(_.nodeClone), exp.nodeClone, env)
}

/* This is removed from the tree during the lambda lifting pass.  */
case class TExpFn(var patterns: List[TExpMatchRow],
                  var funType: TInternalIdentVar)
    extends TExp {
  def prettyPrint = "(fn " + patterns.map(_.prettyPrint).mkString("\n|") + ")"

  def nodeClone =
    new TExpFn(patterns.map(_.nodeClone), funType.nodeClone)
}

/* These are only used after the flatten_let pass.  */
case class TExpAssign(var ident: TNamedIdent, var expression: TExp)
    extends TExp {
  def prettyPrint =
    "Assign %s to (%s)".format(ident.prettyPrint, expression.prettyPrint)

  def nodeClone =
    new TExpAssign(ident.nodeClone, expression.nodeClone)
}

case class TExpListHead(var list: TExp, var tyVar: TInternalIdentVar)
    extends TExp {
  def prettyPrint =
    "Head(%s)".format(list.prettyPrint)

  def nodeClone =
    new TExpListHead(list.nodeClone, tyVar.nodeClone)
}

case class TExpListTail(var list: TExp) extends TExp {
  def prettyPrint =
    "Tail(%s)".format(list.prettyPrint)

  def nodeClone =
    new TExpListTail(list.nodeClone)
}

case class TExpTupleExtract(var tuple: TExp, var tupleSize: Int,
                            var index: Int, var tyVar: TInternalIdentVar)
    extends TExp {
  def prettyPrint =
    "(%s)._%s".format(tuple.prettyPrint, index)

  def nodeClone =
    new TExpTupleExtract(tuple.nodeClone, index, tupleSize, tyVar.nodeClone)
}

case class TExpListExtract(var list: TExp, var index: Int,
                           var tyVar: TInternalIdentVar) extends TExp {
  def prettyPrint =
    "(%s)[%s]".format(list.prettyPrint, index)

  def nodeClone =
    new TExpListExtract(list.nodeClone, index, tyVar.nodeClone)
}

case class TExpListLength(var list: TExp) extends TExp {
  def prettyPrint = "Length of (%s)".format(list.prettyPrint)

  def nodeClone =
    new TExpListLength(list.nodeClone)
}

case class TExpFunLet(var valdecs: Set[TNamedIdent], var exp: TExp)
    extends TExp {
  def prettyPrint = """
    |FunLet
    |%s
    |In
    |%s
    |End""".stripMargin.format(valdecs.map(_.prettyPrint).mkString("\n"),
                               exp.prettyPrint)

  def nodeClone =
    new TExpFunLet(valdecs.map(_.nodeClone), exp.nodeClone)
}

case class TExpIf(var cond: TExp, var ifTrue: TExp, var ifFalse: TExp)
    extends TExp {
  def prettyPrint = """
  |If (%s)
  |Then (%s)
  |Else (%s)""".stripMargin.format(cond.prettyPrint, ifTrue.prettyPrint,
                                   ifFalse.prettyPrint)

  def nodeClone =
    new TExpIf(cond.nodeClone, ifTrue.nodeClone, ifFalse.nodeClone)
}

case class TExpThrow(var throwable: TIdentThrowable) extends TExp {
  def prettyPrint = "throw (%s)".format(throwable.prettyPrint)

  def nodeClone =
    new TExpThrow(throwable.nodeClone)
}
