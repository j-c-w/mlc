package tir

import scala.collection.mutable.Set
import toplev.GenericPrintable

/* This type of program is introduced after after the program_lower
 * pass.  Unlike the TProgram class, this does not contain top level
 * valdecs.  All top level items are functions, with one special
 * function used for a main function.  */

case class TJavaProgram(var typeEnv: TTypeEnv, var main: TJavaFun,
                        var topLevelVariables: Set[TTopLevelIdent],
                        var dataTypeDecs: List[TDataTypeDec],
                        var functions: List[TJavaFun])
    extends TTree {
  def prettyPrint = """
%s

Main: %s

%s
  """.format(topLevelVariables.map(_.prettyPrint).mkString("\n"), 
             main.prettyPrint, functions.map(_.prettyPrint).mkString("\n\n"))

  def nodeClone(env: TTypeEnv): TJavaProgram =
    new TJavaProgram(typeEnv, main.nodeClone(typeEnv), topLevelVariables,
                     dataTypeDecs.map(_.nodeClone(typeEnv)),
                     functions.map(_.nodeClone(typeEnv)))
}
