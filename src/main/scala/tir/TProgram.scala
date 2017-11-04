package tir

import toplev.GenericPrintable

case class TProgram(val typeEnv: TTypeEnv, val funs: List[TFun],
                    val vals: List[TVal]) extends GenericPrintable {
  def prettyPrint: String = """
  %s

  %s

  %s
  """.format(typeEnv.prettyPrint, (funs map (_.prettyPrint)),
             vals map (_.prettyPrint))
}
