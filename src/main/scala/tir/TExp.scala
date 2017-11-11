package tir

import toplev.GenericPrintable
import tpass.TPass

/* An explicit design decision has been made here
 * to parameterize the TWalkalble with TExp.
 *
 * That is, we lose some type saftey for the sake
 * of flexibility (an intuitivity). Any TExp
 * may be replaced by any other TExp.
 */

sealed trait TExp extends TWalkable with GenericPrintable

case class TExpConst(var const: TConst) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    const.walk(item, f)
  }

  def prettyPrint = const.prettyPrint
}

case class TExpIdent(var ident: TIdent) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    ident.walk(item, f)
  }

  def prettyPrint = ident.prettyPrint
}

case class TExpFunApp(var funname: TExp, var application: TExp,
                      var callType: TIdent) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    funname.walk(item, f)
    application.walk(item, f)
  }

  def prettyPrint =
    "(" + funname.prettyPrint + ") (" + application.prettyPrint + ")"
}

case class TExpTuple(var elems: List[TExp]) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    elems.foreach(_.walk(item, f))
  }

  def prettyPrint = "(" + elems.map(_.prettyPrint).mkString(", ") + ")"
}

case class TExpList(var elems: List[TExp]) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    elems.foreach(_.walk(item, f))
  }

  def prettyPrint = "[" + elems.map(_.prettyPrint).mkString(", ") + "]"
}

case class TExpSeq(var seq: List[TExp]) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    seq.foreach(f(item, _))
  }

  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(";\n") + ")"
}

case class TExpLetIn(var decs: List[TDec], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    decs.foreach(_.walk(item, f))
    exp.walk(item, f)
  }

  def prettyPrint =
  """
  |let
  |%s
  |in
  |%s
  |end
  |
  """.stripMargin.format(decs.map(_.prettyPrint).mkString("\n"),
                         exp.prettyPrint)
}

case class TExpCase(var exp: TExp, var cases: List[TExpMatchRow])
    extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    exp.walk(item, f)
    cases.foreach(_.walk(item, f))
  }

  def prettyPrint = """
  |match %s with
  |   %s
  """.stripMargin.format(exp.prettyPrint,
                         cases.map(_.prettyPrint).mkString("\n   |"))
}

case class TExpMatchRow(var pat: List[TPat], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    pat.foreach(_.walk(item, f))
    exp.walk(item, f)
  }


  def prettyPrint =
    pat.map(_.prettyPrint).mkString(" ") + " => " + exp.prettyPrint
}

case class TExpFn(var patterns: List[TExpMatchRow]) extends TExp {
  def walk[T](item: T, f: TPass[T]) = if (f(item, this)) {
    patterns.foreach(_.walk(item, f))
  }

  def prettyPrint = "(fn " + patterns.map(_.prettyPrint).mkString("\n|") + ")"
}
