package tir

import toplev.GenericPrintable

/* This class is employed after the name change class, when
 * we can un-interleave the definition of vals and functions.
 */
case class TProgram(var typeEnv: TTypeEnv, var funs: List[TFun],
                    var vals: List[TVal]) extends GenericPrintable {
  def prettyPrint: String = """
  %s

  %s

  %s
  """.format(typeEnv.prettyPrint, (funs.map(_.prettyPrint)).mkString("\n"),
             vals.map(_.prettyPrint).mkString("\n"))
}
