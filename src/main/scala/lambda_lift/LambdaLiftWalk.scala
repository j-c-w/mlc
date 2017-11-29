package lambda_lift

import ast_change_names.FunctionNameGenerator
import change_names.ChangeIdentNames
import exceptions.ICE
import scala.collection.mutable.HashMap
import tir._
import tpass.TTypeEnvUpdateParentPass
import typecheck.VariableGenerator

class LambdaLiftWalk(val program: TProgram)
    extends TTypeEnvUpdateParentPass {
  var newToplevelFunctions = List[TFun]()

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case let @ TExpLetIn(decs, exp, letEnv) => {
      // This is the only hard case here.
      //
      // We search through this for any decs that
      // are actually function decs and move them back
      // to the top level.
      val valdecs = decs.filter {
        case TVal(_, _) => true
        case _ => false
      } map (_.asInstanceOf[TVal])

      val fundecs = decs.filter {
        case TFun(_, _) => true
        case _ => false
      } map (_.asInstanceOf[TFun])

      // Check if any VALs are actually just FnDecs. Do this
      // by first walking the vals with this (so to extract the
      // annon function definitions).
      //
      // Then check that each val has RHS that is a function
      // identifier.
      //
      // Replace that in the body.
      decs.foreach(apply(letEnv, _))

      // Finally, walk the exp:
      apply(letEnv, exp)

      // For each function, find the free variables
      // and create a lambda lifted function that
      // takes those variables as parameters.
      for (fun <- fundecs) {
        val oldFunctionType = letEnv.getNoSubsituteOrFail(fun.name)
        val (freeValsTuple, freeValsType) =
          insertFunctionFor(fun.name, fun.patterns, letEnv, fun.name)

        // And update any function calls appropriately.
        val funCallUpdateWalk =
          new FunCallUpdateWalk(fun.name, freeValsTuple,
                                freeValsType, oldFunctionType,
                                letEnv)

        // Update the let structure so that the changes
        // propagate back into it. (Resolving a previous bug
        // with:
        // let val x = 1; fun f () = x
        // in f end)
        let.decs = valdecs ::: fundecs
        let.exp = exp
        funCallUpdateWalk((), let)

        // Now, update the function name so that it is a top level
        // function:
        val newName = fun.name match {
          case TIdentVar(name) => TTopLevelIdent(name)
          case other => throw new ICE("""Error: Cannot lift a non
            |ident var""".stripMargin)
        }

        val nameReplacementMap =
          new HashMap[TNamedIdent, (TNamedIdent, TType)]()
        nameReplacementMap(fun.name) = (newName, env.getOrFail(fun.name))
        ChangeIdentNames.newNamesFor(nameReplacementMap, let, letEnv)
      }

      // Update the vals:
      let.decs = valdecs
      None
    }
    case expFn @ TExpFn(patterns, typIdent) => {
      // Get out the old function type before it is substituted.
      val oldFunctionType = env.getNoSubsituteOrFail(typIdent)

      // Create a new function name and a new function at the top level
      // for this.
      val newName = TTopLevelIdent(FunctionNameGenerator.newAnonymousName())

      val (freeValsTuple, freeValsType) =
        insertFunctionFor(newName, patterns, env, typIdent)

      // Create a new type variable reference for the introduced function
      // application.
      val applicationIdent = VariableGenerator.newTVariable()

      env.add(applicationIdent,
              TFunctionType(freeValsType, oldFunctionType), false)

      // The only use of this funciton is where it is declared.
      // Update that use.
      Some(TExpFunApp(TExpIdent(newName), freeValsTuple, applicationIdent))
    }
    case _ => super.apply(env, exp)
  }

  /* This takes the parts of the function provided and it moves
   * the function out to the top level. It does not
   * deal with the deletion of the inner version of the function.
   *
   * 'name' corresponds to the name that the function will have at
   * the top level.
   * 'typeEnvName' corresponds to the name used to get to the function
   * type in the environment.
   */
  def insertFunctionFor(name: TNamedIdent, patterns: List[TExpMatchRow],
                        innerEnv: TTypeEnv, typeEnvName: TIdent) = {
    // First, walk the patterns to see if they have anything that needs
    // to be lambda lifted.
    val newPatterns = patterns.map(apply(innerEnv, _))

    // This stores the merged patterns.
    val updatedPatterns = (patterns zip newPatterns) map {
      case (oldPat, Some(newPat)) => newPat.asInstanceOf[TExpMatchRow]
      case (oldPat, None) => oldPat
    }

    // Now create a tuple type for these variables
    //
    // Note that this does not contain any functions
    // as we assume that those will be lambda lifted.
    val freeValsList = FreeValsWalk(program.typeEnv, patterns)

    // In addition to the above two, we need a plain list of the free
    // val types, a plain list of the freeVal names, and
    // the freeVals tuple as a single expression.
    val freeValsNamesList = freeValsList.map {
      case (TExpIdent(TIdentVar(name)), _) => TIdentVar(name)
      case _ => throw new ICE("""Error: Found a non-tident var in the list of
        free variables""")
    }
    val freeValsTypesList = freeValsList.map {
      case (_, typ) => typ
    }
    val freeValsPatList = freeValsNamesList.map(new TPatVariable(_))
    val freeValsType = new TTupleType(freeValsTypesList)
    val freeValsPatTuple = new TPatSeq(freeValsPatList)
    val freeValsExpTuple =
      new TExpTuple(freeValsNamesList.map(new TExpIdent(_)))

    // And remove the old function type
    val oldFunctionType = innerEnv.getNoSubsituteOrFail(typeEnvName)
    innerEnv.remove(typeEnvName)

    // Add to the top level environment.
    val newFunctionType = new TFunctionType(freeValsType, oldFunctionType)
    program.typeEnv.add(name, newFunctionType, true)

    // Adjust the patterns to take the new arguments:
    updatedPatterns.foreach {
      case matchRow @ TExpMatchRow(pattern, exp, env) => {
        // Update the patterns appropriately here:
        matchRow.pat = freeValsPatTuple :: matchRow.pat

        // And also update the matchRow environment:
        freeValsList.map {
          case (TExpIdent(freeVal), typ: TType) => {
            env.add(freeVal, typ, false)
          }
        }
      }
    }

    // Now re-uniqueify the names of the arguments:
    // Create this in a new function rather than doing this elementwise
    // becaue the ChangeIdentNames function first creatse the new
    // names, and those (obviously) need to be consistent.
    // The environment we pass for the 'new' environment doesn't matter
    // because the walk will replace it with the one in the pattern
    // as a first step anyways.
    ChangeIdentNames.newNamesFor(freeValsNamesList zip freeValsTypesList,
                                 TExpFn(updatedPatterns, name),
                                 innerEnv)

    // Actually make the new function call:
    val newFunction = TFun(name, updatedPatterns)

    // Add this function to the top level:
    newToplevelFunctions = newFunction :: newToplevelFunctions

    (freeValsExpTuple, freeValsType)
  }
}
