package tir

import toplev.GenericPrintable

/* This is a class that represents functions
 * and cases that all have no pattern matching.
 */

case class TSimpleFunctionProgram(var main: TSimpleFun,
  var functions: List[TSimpleFun], var typeEnv: TTypeEnv)
    extends GenericPrintable{
  def prettyPrint = """
  %s

  %s
  """.format(main.prettyPrint, functions.map(_.prettyPrint).mkString("\n"))
}
