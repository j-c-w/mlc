package t_outline

import ast_change_names.FunctionNameGenerator
import exceptions.UnreachableException
import generators.VariableGenerator
import tir._
import tpass.TTypeEnvUpdateParentPass
import type_env.TypeEnvUpdateParentWalk

/* This walk goes through the program and outlines any
 * try-catch blocks.  These need to go in their own functions
 * because the JVM clears the stack on a catch.
 *
 * This does not need to be enabled on other targets.
 */
class OutlineWalk extends TTypeEnvUpdateParentPass {
  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case expTry @ TExpTry(tryExp, catchVar, catchExp, internalTyp) => {
      val functionIdent =
        TIdentVar(FunctionNameGenerator.newAnonymousName(), TFunClass())
      val funAppVar = VariableGenerator.newTInternalVariable() 
      val newLetEnv = new TTypeEnv(Some(env))

      val catchResType = env.getOrFail(internalTyp) match {
        case TFunctionType(from, to) => to
        case _ => throw new UnreachableException()
      }

      newLetEnv.addTopLevel(funAppVar,
                            TFunctionType(TUnitType(), catchResType), false)
      newLetEnv.add(functionIdent, TFunctionType(TUnitType(), catchResType),
                    false)

      // Walk all the cases and set the new parent environment.
      TypeEnvUpdateParentWalk.apply(newLetEnv, expTry)
      super.apply(newLetEnv, expTry)

      val innerFunEnv = new TTypeEnv(Some(newLetEnv))
      TypeEnvUpdateParentWalk.apply(innerFunEnv, expTry)

      // Now create this as a new function:
      Some(TExpLetIn(
        List(TFun(functionIdent, List(TExpMatchRow(List(TPatWildcard()),
          expTry, innerFunEnv)))),
        TExpFunApp(TExpIdent(functionIdent),
                   TExpIdent(TUnitIdent()), funAppVar),
        newLetEnv))
    }
    case expHandle @ TExpHandle(exp, cases, applicationVar) => {
      val functionIdent =
        TIdentVar(FunctionNameGenerator.newAnonymousName(), TFunClass())
      val funAppVar = VariableGenerator.newTInternalVariable() 
      val newLetEnv = new TTypeEnv(Some(env))

      val handleResType = env.getOrFail(applicationVar) match {
        case TFunctionType(from, to) => to
        case _ => throw new UnreachableException()
      }

      newLetEnv.addTopLevel(funAppVar,
                            TFunctionType(TUnitType(), handleResType), false)
      newLetEnv.add(functionIdent, TFunctionType(TUnitType(), handleResType),
                    false)

      // Set all direct children ENVs to have a parent that is the new
      // environment.  This insers 'newLetEnv' into the chain of type
      // environments.
      TypeEnvUpdateParentWalk.apply(newLetEnv, expHandle)
      super.apply(newLetEnv, expHandle)

      val innerFunEnv = new TTypeEnv(Some(newLetEnv))
      TypeEnvUpdateParentWalk.apply(innerFunEnv, expHandle)

      // Now create this as a new function:
      Some(TExpLetIn(
        List(TFun(functionIdent, List(TExpMatchRow(List(TPatWildcard()),
                                                   expHandle, innerFunEnv)))),
        TExpFunApp(TExpIdent(functionIdent),
                   TExpIdent(TUnitIdent()), funAppVar),
        newLetEnv))
    }
    case _ => super.apply(env, exp)
  }
}
