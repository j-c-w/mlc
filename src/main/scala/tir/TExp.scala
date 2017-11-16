package tir

import toplev.GenericPrintable
import tpass.TPass

/* That is, we lose some type saftey for the sake
 * of flexibility (an intuitivity). Any TExp
 * may be replaced by any other TExp.
 */

sealed trait TExp extends GenericPrintable

case class TExpConst(var const: TConst) extends TExp {
  def prettyPrint = const.prettyPrint
}

case class TExpIdent(var ident: TIdent) extends TExp {
  def prettyPrint = ident.prettyPrint
}

case class TExpFunApp(var funname: TExp, var application: TExp,
                      var callType: TIdent) extends TExp {
  def prettyPrint =
    "(" + funname.prettyPrint + ") (" + application.prettyPrint + ")"
}

case class TExpTuple(var elems: List[TExp]) extends TExp {
  def prettyPrint = "(" + elems.map(_.prettyPrint).mkString(", ") + ")"
}

case class TExpList(var elems: List[TExp]) extends TExp {
  def prettyPrint = "[" + elems.map(_.prettyPrint).mkString(", ") + "]"
}

case class TExpSeq(var seq: List[TExp]) extends TExp {
  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(";\n") + ")"
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
}

case class TExpCase(var exp: TExp, var cases: List[TExpMatchRow])
    extends TExp {
  def prettyPrint = """
  |match %s with
  |   %s
  """.stripMargin.format(exp.prettyPrint,
                         cases.map(_.prettyPrint).mkString("\n   |"))
}

case class TExpMatchRow(var pat: List[TPat], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def prettyPrint =
    pat.map(_.prettyPrint).mkString(" ") + " => " + exp.prettyPrint
}

/* This is removed from the tree during the lambda lifting pass.  */
case class TExpFn(var patterns: List[TExpMatchRow], var funType: TIdent)
    extends TExp {
  def prettyPrint = "(fn " + patterns.map(_.prettyPrint).mkString("\n|") + ")"
}

/* These are only used after the flatten_let pass.  */
case class TExpAssign(var ident: TIdentVar, var expression: TExp)
    extends TExp {
  def prettyPrint =
    "Assign %s to (%s)".format(ident.prettyPrint, expression.prettyPrint)
}

case class TExpFunLet(var valdecs: List[TIdentVar],
                      var exp: TExp, var env: TTypeEnv) extends TExp {
  def prettyPrint = """
    |FunLet
    |%s
    |In
    |%s
    |WithTypes:
    |%s
    |End""".stripMargin.format(valdecs.map(_.prettyPrint).mkString("\n"),
                               exp.prettyPrint, env.prettyPrint)
}

case class TExpFunLetMatchRow(var pat: List[TPat], var exp: TExpFunLet,
                              var env: TTypeEnv) extends TExp {
  def prettyPrint = """
%s => %s
  """.format(pat.map(_.prettyPrint).mkString(" "), exp.prettyPrint)
}
