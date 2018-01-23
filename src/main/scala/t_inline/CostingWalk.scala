package t_inline

import def_finder.DefFinder
import exceptions.ICE
import recursion_analysis.RecursionIdentifier
import scala.collection.mutable.{HashMap,HashSet}
import tir._
import toplev.Shared
import tpass.TPass

/* The rough estimate of the cost of a function is based upon:
 *
 * 1. The number of uses of a function.  If a function has many many
 * uses, then those should not all be inlined (note that we must take
 * account of the effect of inlining a function on the number of uses it has).
 *
 * 2. Whether the function is globally visible or not.  If the function
 * is declared within some let binding, then it is not globally visible,
 * and so should be inlined if it has a single use regardless of how many
 * instructions it is likely to generate.
 *
 * 3. Whether the invocation we are replacing is hot or not.
 * Function 'hotness' is decided in a simplistic manner of: is this function
 * is recursive, then it is likely to be hot.  If this function is called
 * from a function that is recursive, then it is likely to be hot.
 *
 * 4. Whether the case statement can be immediately reduced to a single
 * expression.  This is only done for simple cases right now.
 *
 *
 * We assume TargetConfig.maxInlineInstructions(priority) gives us a suitable
 * yields  the right number of instructions to be worth inlining.
 *
 * We keep track of the surrounding function.
 *
 * Bugs:
 *    This does not nessecarily choose the optimal functions to inline
 *    into.  Ideally, this should be preceeded by a walk that counts
 *    the number of uses of a function and uses that idea to prioritize
 *    which functions are inlined into.
 *
 *    This does not take into consideration that functions may be
 *    inlined into functions we are inlining... Oh dear...
 */

class CostingWalk(programEnv: TTypeEnv, program: TProgram)
    extends TPass[Option[TFun], Unit] {
  override def default = ()
  override def combine(u: Unit, v: Unit) = u
  override def combineList(ls: List[Unit]) = ()

  /* This map holds the costs of each function to inline, as an estimate of
   * the number of instructions.  */
  val functionCostsMap = new HashMap[TFun, Int]()
  val shouldInlineMap = new HashMap[TInternalIdentVar, InlineReason]()
  val functionNamesMap = new HashMap[TNamedIdent, TFun]()
  val functionClassificationMap = new HashMap[TNamedIdent, InlinePriority]()

  val walkedSet = new HashSet[TNamedIdent]()

  override def apply(parentFun: Option[TFun], exp: TExp) = exp match {
    case app @ TExpFunApp(fun, expression, typ) => {
      // The parent can be none if this is a top level val declaration.
      val inlineResult = attemptToInline(app, parentFun, programEnv)
      shouldInlineMap(typ) = inlineResult

      // Walk the rest of the application regardless of the result
      // beause it may contain an expression that needs to be inlined.
      super.apply(parentFun, app)
    }
    case other => super.apply(parentFun, other)
  }

  override def apply(parentFun: Option[TFun], dec: TDec) = dec match {
    case fun @ TFun(name, patterns) => {
      if (!walkedSet.contains(name)) {
        walkedSet += (name)
        super.apply(Some(fun), dec)
      }
    }
    case TJavaFun(_, _, _, _) =>
      // This is just a sanity check for the current implementation.
      // There shouldn't be anything wrong with inlinig again after pattern
      // matches have been expanded.  Some testing would be a good idea
      // however.
      throw new ICE("Unexpected TJavaFun in t_inline")
    case other => super.apply(parentFun, other)
  }

  /* This returns whether a function is likely going to be hot or cold.
   * This is done by looking at whether the function is recursive or not if
   * it is recursive, we assume hot.  Otherwise, we assume cold.  */
  def classificationOf(function: Option[TFun]) = {
    if (function == None)
      // This is a top level declaration
      MAYBE_COLD_USE
    else {
      val functionName = function.get.name

      if (functionClassificationMap.contains(functionName)) {
        functionClassificationMap(functionName)
      } else {
        // We may assume that the definition for this function exists
        // since the function as a whole is passed.
        val funDef = definitionFor(functionName)
        assert(funDef != None)
        val hasRecursion =
          RecursionIdentifier.hasRecursion(functionName, funDef.get)

        val classification = if (hasRecursion) {
          MAYBE_HOT_USE
        } else {
          MAYBE_COLD_USE
        }

        functionClassificationMap(functionName) = classification
        classification
      }
    }
  }


  /* This function returns the cost of a function application.  If the
   * application has been inlined, then it returns the value of that
   * function.  If the application has not been inlined, it:
   *
   *  1 - Marks it as no-inline.
   *  2 - Returns a 1.
   *
   * This is to deal with mutually recursive functions.
   *
   * If 'callerName' is none, then this is not part of a function
   * e.g. a top level val dec.  It may then be assemed to be non-recursive.
   */
  def costOfInlining(callerName: Option[TNamedIdent], fun: TFun) = {
    if (functionCostsMap.contains(fun)) {
      functionCostsMap(fun)
    } else if (callerName == Some(fun.name)) {
      // If this is a recursive function, then we do not want to walk it.
      // For the time being, return a cost of Int.MAX
      Int.MaxValue
    } else {
      // We have to make sure to walk the function before thinking
      // about whether we can inline it or not.  This walk takes
      // care of not walking things twice, so we are not at risk
      // of an infinite loop.
      apply(None, fun)
      val cost = FunctionCostWalk((shouldInlineMap), fun)

      functionCostsMap(fun) = cost
      cost
    }
  }

  def definitionFor(functionName: TNamedIdent): Option[TFun] =
    if (functionNamesMap.contains(functionName))
      Some(functionNamesMap(functionName))
    else {
      val (env, fun) =
        DefFinder.getSingleDefOrFail(programEnv, program, functionName)
      // We cannot inline a val.
      fun match {
        case fun : TFun => {
          functionNamesMap(functionName) = fun
          Some(fun)
        }
        case valdec: TVal => None
        case javaFun: TJavaFun =>
          throw new ICE("Inlining after lower program not currently supported")
      }
    }

  /* This function looks at the function application and attempts to decide
   * whether it can be inlined or not.  It returns an appropriate inline
   * setting.
   */
  def attemptToInline(app: TExpFunApp, parentFun: Option[TFun],
                      env: TTypeEnv) = {
    def applicationSearch(app: TExpFunApp, applications: Int,
                          oldInternalIdents: List[TInternalIdentVar],
                          appTypes: List[TType], resType: TType,
                          applicationTuple: List[TExp]): InlineReason =
      app match {
        case TExpFunApp(TExpIdent(name: TTopLevelIdent), exp, internal) => {
          val fun = definitionFor(name)
          val funTypesList = env.getNCurriedTypesFrom(appTypes.length,  name)

          if (fun == None)
            VariableInline(name)
          else if (appTypes != funTypesList)
            // We currently do not support inlining if this is a specialization
            // of a function.  This requires a typechecking pass to be
            // implemented in the TIR, which is something I don't currently
            // have time for.  Expected large perfomance improvements from
            // implementing this because many simple functions are polymorphic.
            //
            // Here, check if the inlined function is a different type to the
            // applicvation types.  If so, then we need to retype the internal
            // application so that cases are correct.
            InlineNotSupported("Inline of polymorphic functions not supported "
                               + "right now.")
          else {
            // In this case, this is the bottom of the recursion.
            // If this is the full recursion, then apply everything to
            // it.
            if (fun.get.curriedArgumentsCount == applications + 1) {
              // This is the full application, we can reduce it here.
              val cost = costOfInlining(parentFun.map(_.name), fun.get)
              val thresh =
                Shared.targetConfig.maxInlineInstructions(
                  classificationOf(parentFun))

              if (cost <= thresh) {
                val varsSet = HashSet[TInternalIdentVar]()
                varsSet ++= internal :: oldInternalIdents

                val thisResType = env.getOrFail(internal) match {
                  case TFunctionType(arg, _) => arg
                  case other => throw new ICE("Function without function type")
                }

                // We need to avoid creating a length 0 tuple.
                val argTypes: TType =
                  if (appTypes.length == 0)
                    thisResType
                  else
                    TTupleType(thisResType :: appTypes)

                Inline(cost, thresh, name, exp :: applicationTuple,
                       TFunctionType(argTypes, resType), varsSet)
              } else {
                InlineTooBigFailure(name, cost, thresh)
              }
            } else {
              InlinePartialApplicationFailure(name)
            }
          }
        }
        case TExpFunApp(TExpIdent(longIdent: TIdentLongVar), _, _) =>
          InlineNotSupported("""Cross module inlining not supported yet.
            | Not inlining %s.""".stripMargin.format(longIdent.prettyPrint))
        // This is likely some local variable being used as a function
        // application.  We cannot inline that.
        case TExpFunApp(TExpIdent(id: TNamedIdent), _, _) => VariableInline(id)
        case TExpFunApp(fun: TExpFunApp, exp: TExp,
                        internal: TInternalIdentVar) => {
          val functionType = env.getOrFail(internal) match {
            case TFunctionType(from, to) => from
            case other => throw new ICE("Function type without function type.")
          }

          applicationSearch(fun, applications + 1,
                            internal :: oldInternalIdents,
                            functionType :: appTypes, resType,
                            exp :: applicationTuple)
        }
        // This is probably some builtin like + * etc.
        case TExpFunApp(TExpIdent(ident), _, _) =>
          UninlinableFunction(ident)
        case app @ TExpFunApp(fun: TExp, exp: TExp,
                              internal: TInternalIdentVar) =>
          // There is probably nothing wrong with this case.
          // However, currently, there is no arbitrary expression
          // that can generate a function after lambda lifting.
          UninlineableExpression(fun)
      }

    // Check if a decision has already been made about this function.
    // Mutually recursive functions can decide to inline into each other
    // if we are not careful.
    if (shouldInlineMap.contains(app.callType)) {
      shouldInlineMap(app.callType)
    } else {
      val resType = programEnv.getOrFail(app.callType) match {
        case TFunctionType(_, res) => res
        case _ => throw new ICE("Function has non function type")
      }

      applicationSearch(app, 0, List(), List(), resType, List())
    }
  }
}
