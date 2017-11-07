package tir

import toplev.GenericPrintable

/* The original lowering pass lowers to this class. This
 * class preserves the original declaration order. Function
 * types and val types are split into separate lists
 * once the NameChange pass has run. */
case class TProgramOrdered(val typeEnv: TTypeEnv, val decs: List[TDec])
    extends GenericPrintable {
  def prettyPrint = """

  %s

  %s
  """.format(typeEnv.prettyPrint, decs.map(_.prettyPrint).mkString("\n"))
}
