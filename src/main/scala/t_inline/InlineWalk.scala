package t_inline

import alpha_rename.AlphaRename
import def_finder.DefFinder
import exceptions.ICE
import scala.collection.mutable.{HashMap,Map}
import t_typecheck.TTypecheck
import tir._
import tpass.TTypeEnvUpdateParentPass
import typecheck.VariableGenerator

class InlineWalk(program: TProgram,
                 inlineMap: Map[TInternalIdentVar, InlineReason],
                 dumpFunction: String => Unit)
    extends TTypeEnvUpdateParentPass {
  // This is dumped as part of the output file for this pass.
  var inlineCount = 0

  val functionDefs =
    new HashMap[TNamedIdent, (TExp, TFunctionType) => TExpCase]()

  /* Since the same functions are likely to be inlined over and over again,
   * it is sensible to cache them in a map.
   *
   * There is no need to update the function defs cost when a function is
   * updated as the mutable pointers will mean that the cached function is
   * also updated.
   */
  private def getFunctionDef(name: TNamedIdent) = {
    functionDefs.get(name) match {
      case Some(fun) => fun
      case None => {// Use the DefFinder to get the function.
        val funDef = DefFinder(program.typeEnv, program, name)

        funDef match {
          case Some((env, fun @ TFun(funName, patterns))) => {
            // This is the first serious use of the def finder.
            // Sanity check the result.
            assert(funName == name)

            // Transform the function.  The idea is to do:
            //  fun f pat  pat  pat  = exp
            //    | f pat' pat' pat' = exp'
            //
            // Into:
            //  case __ of
            //    (pat, pat, pat) = exp
            //    | (pat', pat', pat') = exp'

            val newPatterns = patterns.map {
              case row @ TExpMatchRow(pat, exp, env) => {
                if (pat.length > 1) {
                  TExpMatchRow(List(TPatSeq(pat)), exp, env)
                } else {
                  row
                }
              }
            }

            def caseGenerator(application: TExp, appType: TFunctionType) = {
              val interalApplicationVar =
                VariableGenerator.newTInternalVariable()
              // Add the app type to the program
              program.typeEnv.addTopLevel(interalApplicationVar, appType,
                                          false)

              // The case statement must be alpha renamed to keep all bound
              // variables unique.  The pass type environment is not relevant
              // here as it will be reset before anything is changed.
              val generatedPatterns =
                newPatterns.map((node: TExpMatchRow) =>
                     AlphaRename.rename(node.nodeClone(program.typeEnv),
                                        node.env)
                                          .asInstanceOf[TExpMatchRow])

              // We have to make sure to change the types of the variables
              // within this new application correctly.  This is done
              // by re-typing each exp row with the specified types in mind.
              // Note that this must be done after the patterns are mapped.
              // val typedPatterns =
              //   generatedPatterns.map {
              //     case matchRow =>
              //       TTypecheck.specialzeRowPattern(matchRow, appType)
              //   }
              //   This is commented out because inlining of polymorphic
              //   functions is not supported right now.

              TExpCase(application, generatedPatterns, interalApplicationVar)
            }

            functionDefs(name) = caseGenerator _
            caseGenerator _
          }
          case Some((env, notFun)) =>
            throw new ICE("""Attempting to inline %s, which is not
              |a function""".stripMargin.format(notFun.prettyPrint))
          case None =>
            throw new ICE("""Trying to inline function that does not exist.
              | Function is %s""".stripMargin.format(name.prettyPrint))
        }
      }
    }
  }

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case funApp @ TExpFunApp(fun, app, typ) => {
      inlineMap(typ) match {
        case inline @ Inline(cost, threshold, funName, newExp,
                             appType, oldAppVars) => {
          // Walk the new case statement in case any of the sub
          // components of this function need to be inlined.
          //
          // This must be done first because this call might change
          // the internal names of the case statement.
          super.apply(env, funApp) match {
            case Some(changedStmt) =>
              // This is unexpected because the parent class shouldn't  be
              // changing anything.
              throw new ICE("Unexpected change to case statement")
            case None => // This is OK
          }

          // Replace the function with the inlined version:
          val caseGenerator = getFunctionDef(funName)
          // Create the replacement expression:

          assert(newExp.length != 0)
          val caseExp =
            if (newExp.length == 1)
              newExp(0)
            else
              TExpTuple(newExp)

          val caseStmt = caseGenerator(caseExp, appType)

          inlineCount += 1
          dumpFunction("Inlining of " + funName.prettyPrint + " succeeded. " +
                       inline.prettyPrint + "\n")

          Some(caseStmt)
        }
        case failureReason => {
          dumpFunction("Inlining of " + fun.prettyPrint + " failed because: " +
                       failureReason.prettyPrint + "\n")

          // One of the parts of the function application could have been
          // requested as inline.
          super.apply(env, funApp)
        }
      }
    }
    case other => super.apply(env, other)
  }
}
