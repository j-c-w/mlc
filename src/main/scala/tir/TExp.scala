package tir

import scala.collection.mutable.Set
import toplev.GenericPrintable
import tpass.TPass

/* That is, we lose some type saftey for the sake
 * of flexibility (an intuitivity). Any TExp
 * may be replaced by any other TExp.
 */

sealed trait TExp extends TTree {
  def nodeClone(env: TTypeEnv): TExp
}

case class TExpConst(var const: TConst) extends TExp {
  def prettyPrint = const.prettyPrint

  def nodeClone(env: TTypeEnv) = new TExpConst(const.nodeClone(env))
}

case class TExpIdent(var ident: TIdent) extends TExp {
  def prettyPrint = ident.prettyPrint

  def nodeClone(env: TTypeEnv) = new TExpIdent(ident.nodeClone(env))
}

case class TExpFunApp(var funname: TExp, var application: TExp,
                      var callType: TInternalIdentVar) extends TExp {
  def prettyPrint =
    "(" + funname.prettyPrint + ") (" + application.prettyPrint + ")"

  def nodeClone(env: TTypeEnv) =
    new TExpFunApp(funname.nodeClone(env), application.nodeClone(env),
                   callType.nodeClone(env))
}

case class TExpTuple(var elems: List[TExp])
    extends TExp with TFlattenable[TExp] {
  // There are lots of other checks for singleton tuples.  If single tuples are needed
  // temporarily, it would not be a huge loss to delete this.
  assert(elems.length > 1)

  def prettyPrint = "(" + elems.map(_.prettyPrint).mkString(", ") + ")"

  def nodeClone(env: TTypeEnv) =
    new TExpTuple(elems.map(_.nodeClone(env)))

  def flatten =
    if (elems.length == 1)
      elems(0) match {
        case flattenable: TFlattenable[TExp] @unchecked => flattenable.flatten
        case other => other
      }
    else
      TExpTuple(elems.map {
        case flattenable: TFlattenable[TExp] @unchecked => flattenable.flatten
        case other => other
      })
}

case class TExpList(var elems: List[TExp]) extends TExp {
  def prettyPrint = "[" + elems.map(_.prettyPrint).mkString(", ") + "]"

  def nodeClone(env: TTypeEnv) =
    new TExpList(elems.map(_.nodeClone(env)))
}

case class TExpSeq(var seq: List[TExp]) extends TExp with TFlattenable[TExp] {
  assert(seq.length > 1)
  def prettyPrint = "(" + seq.map(_.prettyPrint).mkString(";\n") + ")"

  def nodeClone(env: TTypeEnv) =
    new TExpSeq(seq.map(_.nodeClone(env)))

  def flatten = if (seq.length == 1)
      seq(0) match {
        case flattenable: TFlattenable[TExp] @unchecked => flattenable.flatten
        case other => other
      }
    else
      TExpSeq((seq.map(recursiveFlatten(_))).flatten)

  private def recursiveFlatten(exp: TExp): List[TExp] = exp match {
      case TExpSeq(subElems) => subElems.map(recursiveFlatten(_)).flatten
      case flattenable: TFlattenable[TExp] @unchecked =>
        List(flattenable.flatten)
      case other => List(other)
    }
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
  def nodeClone(env: TTypeEnv) =
    new TExpLetIn(decs.map(_.nodeClone(env)), exp.nodeClone(env), env)
}

// Note that application type is a function type here, with
// type from exp -> cases.
case class TExpCase(var exp: TExp, var cases: List[TExpMatchRow],
                    var applicationType: TInternalIdentVar)
    extends TExp {
  def prettyPrint = """
  |match %s with
  |   %s
  |type: %s
  """.stripMargin.format(exp.prettyPrint,
                         cases.map(_.prettyPrint).mkString("\n   |"),
                         applicationType.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpCase(exp.nodeClone(env), cases.map(_.nodeClone(env)),
                 applicationType.nodeClone(env))
}

case class TExpMatchRow(var pat: List[TPat], var exp: TExp, var env: TTypeEnv)
    extends TExp {
  def prettyPrint =
    pat.map(_.prettyPrint).mkString(" ") + " => " + exp.prettyPrint

  def nodeClone(env: TTypeEnv) =
    new TExpMatchRow(pat.map(_.nodeClone(env)), exp.nodeClone(env), env)
}

/* This is removed from the tree during the lambda lifting pass.  */
case class TExpFn(var patterns: List[TExpMatchRow],
                  var funType: TInternalIdentVar)
    extends TExp {
  def prettyPrint = "(fn " + patterns.map(_.prettyPrint).mkString("\n|") + ")"

  def nodeClone(env: TTypeEnv) =
    new TExpFn(patterns.map(_.nodeClone(env)), funType.nodeClone(env))
}

/* These are only used after the lower_program pass.  */
case class TExpAssign(var ident: TNamedIdent, var expression: TExp)
    extends TExp {
  def prettyPrint =
    "Assign %s to (%s)".format(ident.prettyPrint, expression.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpAssign(ident.nodeClone(env), expression.nodeClone(env))
}

case class TExpListHead(var list: TExp, var tyVar: TInternalIdentVar)
    extends TExp {
  def prettyPrint =
    "Head(%s)".format(list.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpListHead(list.nodeClone(env), tyVar.nodeClone(env))
}

case class TExpListTail(var list: TExp) extends TExp {
  def prettyPrint =
    "Tail(%s)".format(list.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpListTail(list.nodeClone(env))
}

case class TExpTupleExtract(var tuple: TExp, var tupleSize: Int,
                            var index: Int, var tyVar: TInternalIdentVar)
    extends TExp {
  assert(tupleSize > 1)
  assert(index < tupleSize)

  def prettyPrint =
    "(%s)._%s".format(tuple.prettyPrint, index)

  def nodeClone(env: TTypeEnv) =
    new TExpTupleExtract(tuple.nodeClone(env), index, tupleSize,
                         tyVar.nodeClone(env))
}

case class TExpListExtract(var list: TExp, var index: Int,
                           var tyVar: TInternalIdentVar) extends TExp {
  def prettyPrint =
    "(%s)[%s]".format(list.prettyPrint, index)

  def nodeClone(env: TTypeEnv) =
    new TExpListExtract(list.nodeClone(env), index, tyVar.nodeClone(env))
}

case class TExpListLength(var list: TExp) extends TExp {
  def prettyPrint = "Length of (%s)".format(list.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpListLength(list.nodeClone(env))
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

  def nodeClone(env: TTypeEnv) =
    new TExpFunLet(valdecs.map(_.nodeClone(env)), exp.nodeClone(env))
}

case class TExpIf(var cond: TExp, var ifTrue: TExp, var ifFalse: TExp)
    extends TExp {
  def prettyPrint = """
  |If (%s)
  |Then (%s)
  |Else (%s)""".stripMargin.format(cond.prettyPrint, ifTrue.prettyPrint,
                                   ifFalse.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpIf(cond.nodeClone(env), ifTrue.nodeClone(env),
               ifFalse.nodeClone(env))
}

case class TExpThrow(var throwable: TIdentThrowable) extends TExp {
  def prettyPrint = "throw (%s)".format(throwable.prettyPrint)

  def nodeClone(env: TTypeEnv) =
    new TExpThrow(throwable.nodeClone(env))
}
