package tir

import toplev.GenericPrintable

/* An explicit design decision has been made here
 * to parameterize the TWalkalble with TExp.
 *
 * That is, we lose some type saftey for the sake
 * of flexibility (an intuitivity). Any TExp
 * may be replaced by any other TExp.
 */

sealed trait TExp extends TWalkable with GenericPrintable

case class TExpConst(var const: TConst) extends TExp {
  def walk(f: TPass) = f(this)
  def prettyPrint = const.prettyPrint
}

case class TExpIdent(var ident: TIdent) extends TExp {
  def walk(f: TPass) = f(this)
  def prettyPrint = ident.prettyPrint
}

case class TExpFunApp(var funname: TExp, var application: TExp,
                      var callType: TIdent) extends TExp {
  def walk(f: TPass) = if (f(this)) {
    funname.walk(f)
    application.walk(f)
  }

  def prettyPrint =
    "(" + funname.prettyPrint + ") (" + application.prettyPrint + ")"
}

case class TExpTuple(var elems: List[TExp]) extends TExp {
  def walk(f: TPass) = if (f(this)) {
    elems.foreach(_.walk(f))
  }

  def prettyPrint = "(" + elems.map(_.prettyPrint).mkString(", ") + ")"
}

case class TExpList(var elems: List[TExp]) extends TExp {
  def walk(f: TPass) = if (f(this)) {
    elems.foreach(_.walk(f))
  }

  def prettyPrint = "[" + elems.map(_.prettyPrint).mkString(", ") + "]"
}

case class TExpSeq(var seq: List[TExp]) extends TExp {
  def walk(f: TPass) = if (f(this)) {
    seq.foreach(f(_))
  }

  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(";\n") + ")"
}

case class TExpLetIn(var decs: List[TDec], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def walk(f: TPass) = if (f(this)) {
    decs.foreach(_.walk(f))
    exp.walk(f)
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
  def walk(f: TPass) = if (f(this)) {
    exp.walk(f)
    cases.foreach(_.walk(f))
  }

  def prettyPrint = """
  |match %s with
  |   %s
  """.stripMargin.format(exp.prettyPrint,
                         cases.map(_.prettyPrint).mkString("\n   |"))
}

case class TExpMatchRow(var pat: List[TPat], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def walk(f: TPass) = if (f(this)) {
    pat.foreach(_.walk(f))
    exp.walk(f)
  }


  def prettyPrint =
    pat.map(_.prettyPrint).mkString(" ") + " => " + exp.prettyPrint
}

case class TExpFn(var patterns: List[TExpMatchRow]) extends TExp {
  def walk(f: TPass) = if (f(this)) {
    patterns.foreach(_.walk(f))
  }

  def prettyPrint = "(fn " + patterns.map(_.prettyPrint).mkString("\n|") + ")"
}
