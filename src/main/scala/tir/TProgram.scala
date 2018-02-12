package tir

import toplev.GenericPrintable
import tpass.TPass

/* This class is employed after the name change class, when
 * we can un-interleave the definition of vals and functions.
 */
case class TProgram(var typeEnv: TTypeEnv,
                    var dataTypeDecs: List[TDataTypeDec], var funs: List[TFun],
                    var vals: List[TVal]) extends TTree {
  def prettyPrint: String = """
  %s


  %s

  %s

  %s
  """.format(typeEnv.prettyPrint,
             (dataTypeDecs.map(_.prettyPrint)).mkString("\n"),
             (funs.map(_.prettyPrint)).mkString("\n"),
             vals.map(_.prettyPrint).mkString("\n"))

  def nodeClone(env: TTypeEnv) =
    new TProgram(typeEnv, dataTypeDecs.map(_.nodeClone(typeEnv)),
                 funs.map(_.nodeClone(typeEnv)),
                 vals.map(_.nodeClone(typeEnv)))
}
