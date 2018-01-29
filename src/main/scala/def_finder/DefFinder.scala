package def_finder

import exceptions.ICE
import tir._

/* This is left as a shell implementation in the hope that it can be
 * made faster by caching results.
 *
 * The tough problem is figuring out how to invalidate cached
 * results.
 */

object DefFinder {
  def apply(env: TTypeEnv, program: TTree, ident: TNamedIdent) = {
    new DefFinderWalk(ident).apply(env, program)
  }

  def getSingleDef(env: TTypeEnv, program: TTree, ident: TNamedIdent) = {
    apply(env, program, ident) match {
      case Nil => None
      case List(definition) => Some(definition)
      case other => None
    }
  }

  /* This searches for definitions.  A definition is some ValDec.  */
  def getSingleDefOrFail(env: TTypeEnv, program: TTree, ident: TNamedIdent) = {
    apply(env, program, ident) match {
      case Nil => throw new ICE("No def found for identifier " + ident)
      case List(definition) => definition
      case other =>
        throw new ICE("Found multiple defs for identifier " + ident)
    }
  }

  /* This searches for assignments to variables.  */
  def getAssigns(env: TTypeEnv, program: TTree, ident: TNamedIdent) = {
    new AssignmentFinderWalk(ident).apply(env, program)
  }

  def getSingleAssign(env: TTypeEnv, program: TTree, ident: TNamedIdent) = {
    getAssigns(env, program, ident) match {
      case Nil => None
      case List(assign) => Some(assign)
      case other => None
    }
  }

  def getSingleAssignOrFail(env: TTypeEnv, program: TTree,
                            ident: TNamedIdent) = {
    getAssigns(env, program, ident) match {
      case Nil => None
      case List(assign) => assign
      case other =>
        throw new ICE("Found multiple assignments for " + ident)
    }
  }
}
