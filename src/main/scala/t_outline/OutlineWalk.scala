package t_outline

import ast_change_names.FunctionNameGenerator
import exceptions.UnreachableException
import generators.VariableGenerator
import tir._
import tpass.TTypeEnvUpdateParentPass

/* This walk goes through the program and outlines any
 * try-catch blocks.  These need to go in their own functions
 * because the JVM clears the stack on a catch.
 *
 * This does not need to be enabled on other targets.
 */
class OutlineWalk extends TTypeEnvUpdateParentPass {
  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case expTry @ TExpTry(tryExp, catchVar, catchExp, internalTyp) => {
      super.apply(env, expTry)

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

      // Now create this as a new function:
      Some(TExpLetIn(
        List(TFun(functionIdent, List(TExpMatchRow(List(TPatWildcard()),
          expTry, new TTypeEnv(Some(newLetEnv)))))),
        TExpFunApp(TExpIdent(functionIdent),
                   TExpIdent(TUnitIdent()), funAppVar),
        newLetEnv))
    }
    case expHandle @ TExpHandle(exp, cases, applicationVar) => {
      super.apply(env, expHandle)

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

      // Now create this as a new function:
      Some(TExpLetIn(
        List(TFun(functionIdent, List(TExpMatchRow(List(TPatWildcard()),
          expHandle, new TTypeEnv(Some(newLetEnv)))))),
        TExpFunApp(TExpIdent(functionIdent),
                   TExpIdent(TUnitIdent()), funAppVar),
        newLetEnv))
    }
    case _ => super.apply(env, exp)
  }
}
