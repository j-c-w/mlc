package tir

import toplev.GenericPrintable

/* This type of program is introduced after after the program_lower
 * pass.  Unlike the TProgram class, this does not contain top level
 * valdecs.  All top level items are functions, with one special
 * function used for a main function.  */

case class TJavaProgram(var typeEnv: TTypeEnv, var main: TJavaFun,
                        var functions: List[TJavaFun])
    extends TTree {
  def prettyPrint = """
Main: %s

%s
  """.format(main.prettyPrint, functions.map(_.prettyPrint).mkString("\n\n"))
}
