package lambda_lift

import exceptions.ICE
import tir._
import tpass.TPass
import typecheck.VariableGenerator

/*  This class is used to walk to the tree after a function has been lifted.
 *
 *  It searches for uses of that function in the following expressions,
 *  and replaces calls to that function with the same call, but
 *  with an original curried call to the new params.
 *
 *  So:
 *
 *  fun f x y z = x + y
 *
 *  let val x = 1
 *  in
 *    f 1
 *  end
 *
 *  becomes
 *
 *  fun f x y z = x + y
 *  let val x = 1
 *  in
 *    f x 1
 *  end
 *
 * Note that this is done in conjuction with the 'lifing' part of
 * lambda lifting.
 *
 * This pass replaces one on closures, as it allows us to treat a partial
 * closure as a partial function application!
 */

class FunCallUpdateWalk(val funID: TIdentVar, val newParams: TExp,
                        val newParamsType: TType, val oldFunctionType: TType,
                        val env: TTypeEnv)
    extends TPass[Unit, Unit] {
  def default = ()
  def combine(x: Unit, y: Unit) = x

  override def apply(u: Unit, expression: TExp) = expression match {
    case parent @ TExpFunApp(funName, application, callType) => {
      val replacedFun =
        updateFunCall(funName, (newCall) => parent.funname = newCall)
      val replacedApp =
        updateFunCall(application, (newCall) => parent.application = newCall)

      if (!replacedFun) {
        apply((), funName)
      }
      if (!replacedApp) {
        apply((), application)
      }
    }
    case parent @ TExpTuple(elems) => {
      parent.elems = elems.map((elem) => {
        val (replaced, newItem) = updateFunCallReturn(elem)

        if (!replaced) {
          apply((), newItem)
        }

        newItem
      })
    }
    case parent @ TExpSeq(seq) => {
      parent.seq = seq.map((item) => {
        val (replaced, newItem) = updateFunCallReturn(item)

        if (!replaced) {
          apply((), newItem)
        }

        newItem
      })
    }
    case parent @ TExpLetIn(decs, exp, env) => {
      // The decs cannot directly be uses of this function.
      decs.foreach(apply((), _))

      val replacedExp = updateFunCall(exp, (newExp) => parent.exp = newExp)

      if (!replacedExp) {
        apply((), exp)
      }
    }
    case parent @ TExpCase(exp, cases) => {
      // The cases cannot directly be the function:
      cases.foreach(apply((), _))

      val replacedExp = updateFunCall(exp, (newExp) => parent.exp = newExp)

      if (!replacedExp) {
        apply((), exp)
      }
    }
    case parent @ TExpMatchRow(pat, exp, env) => {
      // The pattern cannot contain the function call at all.
      val replacedExp = updateFunCall(exp, (newExp) => parent.exp = newExp)

      if (!replacedExp) {
        apply((), exp)
      }
    }
    case TExpFn(cases, typ) => throw new ICE("""Error: TExpFn reached.
      |This should have already been eliminated.""".stripMargin)
    case _ => super.apply((), expression)
  }

  def updateFunCall(exp: TExp, postUpdate: (TExp => Unit)): Boolean = {
    val (wasUpdated, newExp) = updateFunCallReturn(exp)

    if (wasUpdated) {
      postUpdate(newExp)
    }

    wasUpdated
  }

  /* This generates a new expression to slot into the tree.  */
  def updateFunCallReturn(exp: TExp): (Boolean, TExp) = exp match {
    case expIdent @ TExpIdent(otherIdent) => otherIdent match {
      case TIdentVar(name) => if (name == funID.name) {
        // We must add the call type to the top level environment here.
        val callType = TFunctionType(newParamsType, oldFunctionType)
        val callTypeIdent = VariableGenerator.newTVariable()

        env.addTopLevel(callTypeIdent, callType, false)

        (true, TExpFunApp(expIdent, newParams, callTypeIdent))
      } else {
        (false, exp)
      }
      case other => (false, expIdent)
    }
    case other => (false, other)
  }
}

