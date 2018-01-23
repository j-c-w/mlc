package recursion_analysis

import def_finder.DefFinder
import exceptions.ICE
import tir._

object RecursionIdentifier {
  def hasRecursion(env: TTypeEnv, program: TProgram,
                   name: TNamedIdent): Boolean = {
    val (_, funDef) = DefFinder.getSingleDefOrFail(env, program, name)
    hasRecursion(name, funDef)
  }
  def hasRecursion(name: TNamedIdent, dec: TDec): Boolean =
    new RecursionIdentifierWalk(name).apply((), dec)
}
