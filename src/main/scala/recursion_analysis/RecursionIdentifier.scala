package recursion_analysis

import def_finder.DefFinder
import exceptions.ICE
import tir._

object RecursionIdentifier {
  def hasRecursion(env: TTypeEnv, program: TProgram,
                   name: TNamedIdent): Boolean = {
    val (_, funDef) = DefFinder(env, program, name).getOrElse(
      throw new ICE("Requested identifier " + name + " does not exist"))
    hasRecursion(name, funDef)
  }
  def hasRecursion(name: TNamedIdent, dec: TDec): Boolean =
    new RecursionIdentifierWalk(name).apply((), dec)
}
