package tail_call_elim

import function_analysis.FunctionAnalysis
import recursion_analysis.TParentSetTailPass
import tir._

/* This walk goes through and replaces all tail calls the function
 * 'name' with assignments to the loopVars followed  by 'TContinue'
 * nodes.
 *
 * It likely needs to be used on conjunction with the ReplaceResultWalk
 * walk.  */

class ReplaceTailCallWalk(name: TNamedIdent, loopVars: List[TMutableIdent],
                          env: TTypeEnv, whileLoopID: Int)
    extends TParentSetTailPass  {
  override def apply(tailCall: Boolean, exp: TExp) = exp match {
    // This is the hard case for this walk.  We need to determine whether
    // this is a tail recursive call to this function.  If it is, then we
    // should replace it with assignments and continue.
    case app @ TExpFunApp(fun, appExp, typ) => {
      val applicationExpressions =
        FunctionAnalysis.getFullApplicatonFrom(name, loopVars.length, env, app)

      applicationExpressions.map(_.unzip) match {
        case Some((applicationExpressions, types)) => {
          // We require that this is a tail call (although with a better
          // costing mechanism we could replace partially tail recursive
          // functions).
          // assert(tailCall)
          assert(applicationExpressions.length == loopVars.length)

          // Then replace it.
          val assignments =
            (applicationExpressions zip loopVars) map {
              case (exp, varName) =>
                TExpAssign(varName.nodeClone(env), exp)
            }
          val continueExp = TExpContinue(whileLoopID)
          Some(TExpSeq(assignments :+ continueExp))
        }
        // This was not a full application of this function.
        case None => {
          super.apply(tailCall, exp)
        }
      }
    }
    case other => super.apply(tailCall, other)
  }
}
