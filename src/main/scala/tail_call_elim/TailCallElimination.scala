package tail_call_elim

import generators.{IDGenerator,VariableGenerator}
import recursion_analysis.RecursionAnalysis
import tir._
import tir_utils.PatConverter
import toplev.OptionalPass

/* This class elimiates tail recursive calls.  */

object TailCallElimination extends OptionalPass[TProgram]("tail_elim") {
  var numberProcessed = 0

  /* The idea is the replace a function that looks like this:
   *   fun f x y z = e1
   *     | f a b c = e2
   *     | ...
   *
   * With:
   *   fun f x y z =
   *     while (true):
   *       case (x, y, z) of
   *           x y z => e1
   *         | a b c => e2
   *         | ...
   *
   * Where tail recursive calls are replaced with assignments
   * to x, y, z and anything that would previously have returned
   * is replaced with an explcit return call.  */
  def eliminateRecursionFrom(programEnv: TTypeEnv, fun: TFun) = fun match {
    case TFun(name, patterns) => {
      dumpString(
        "Eliminating tail recusion from %s\n".format(name.prettyPrint))
      numberProcessed += 1

      assert(patterns.length > 0)
      val curriedVars = patterns(0).pat.map {
        case _ => VariableGenerator.newTMutableVariable(new TValClass())
      }

      val internalCaseTyp = VariableGenerator.newTInternalVariable()

      // Un-curry the match rows.
      val newMatchRows = patterns.map {
        case TExpMatchRow(pat, exp, env) =>
          TExpMatchRow(List(PatConverter.decurry(pat)), exp, env)
      }

      // Create a tuple of the values we can use as an assignment.
      val curriedVarsTuple =
        if (curriedVars.length > 1)
          TExpTuple(curriedVars.map(new TExpIdent(_)))
        else
          TExpIdent(curriedVars(0))

      // Add the type to the top level.  The application type of the
      // case statement is indeed the same as the function type with
      // the function swapped for a tuple.
      val (matchTypeList, matchResType) =
        programEnv.splitFunctionAt(curriedVars.length, name)
      assert(matchTypeList.length > 0)
      val matchType =
        if (matchTypeList.length == 1) {
          matchTypeList(0)
        } else {
          new TTupleType(matchTypeList)
        }
      val matchTypeWithResult =
        new TFunctionType(matchType, matchResType)

      programEnv.addTopLevel(internalCaseTyp, matchTypeWithResult, false)

      val curriedLength = patterns(0).pat.length

      val caseStatement =
        TExpCase(curriedVarsTuple, newMatchRows, internalCaseTyp)

      val whileID = IDGenerator.newWhileLoopID
      // Now we must construct the while loop:
      val whileLoop = TExpWhile(TExpConst(TConstTrue()), caseStatement,
                                whileID)

      // Now we insert the return statements where the loop would terminate
      // and insert continue statements elsewhere.
      val replacementWalk =
        new ReplaceTailCallWalk(name, curriedVars,
                                programEnv, whileID)
      replacementWalk.apply(true, whileLoop)

      // Replace the results in this with return statements.  Note that this
      // must be done after the ReplaceTailCallWalk has been run to avoid
      // inserting returns around tail calls.
      val replaceResultWalk = new ReplaceResultWalk(whileID)
      replaceResultWalk.apply(true, caseStatement)


      val curriedVarPat =
        curriedVars map (new TPatVariable(_))

      val funTypes =
        programEnv.getNCurriedTypesFrom(curriedLength, name)
      assert(funTypes.length == curriedVars.length)

      // The new environment needs to have the top level as it's parent.
      // Further, it needs to have all the function variables defined.
      val newEnv = new TTypeEnv(Some(programEnv))
      (funTypes zip curriedVars) foreach {
        case (typ, name) => newEnv.add(name, typ, false)
      }

      // We also have to update the parent environments of any of the
      // *immediate* children (the rest will be updated implictly).
      newMatchRows.foreach {
        case matchRow => matchRow.env.parent = Some(newEnv)
      }

      fun.patterns = List(TExpMatchRow(curriedVarPat, whileLoop, newEnv))
    }
  }

  override def run(tree: TProgram) = {
    numberProcessed = 0

    // First get all the tail recursive function calls:
    val tailFunctions =
      tree.funs.filter(RecursionAnalysis.isTailRecursive(_, tree.typeEnv))

    // Go through and eliminate the tail recursion:
    tailFunctions.foreach(eliminateRecursionFrom(tree.typeEnv, _))

    dumpString("Number of functions replaced = %s".format(numberProcessed))
    tree
  }
}
