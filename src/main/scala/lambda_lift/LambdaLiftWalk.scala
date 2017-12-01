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
      // We search through this for any decs that are actually function decs
      // and move them back to the top level.
      var valdecs = decs.filter {
        case TVal(_, _) => true
        case _ => false
      } map (_.asInstanceOf[TVal])

      val fundecs = decs.filter {
        case TFun(_, _) => true
        case _ => false
      } map (_.asInstanceOf[TFun])

      // Check if any VALs are actually just FnDecs. Do this by first walking
      // the vals with this (so to extract the annon function definitions).
      //
      // Then check that each val has RHS that is a function
      // identifier.
      //
      // Replace that in the body.
      decs.foreach(apply(letEnv, _))

      // Finally, walk the exp:
      apply(letEnv, exp)

      // For each function, find the free variables and create a lambda lifted
      // function that takes those variables as parameters.
      //
      // Add that as a valdec to the list of valdecs with the closure
      // arguments applied to it.
      for (fun <- fundecs) {
        val oldFunctionType = letEnv.getNoSubstituteOrFail(fun.name)
        val oldName = fun.name
        // Then set the function to have a new top level name
        // and update the uses of that function:
        val newName = fun.name match {
          case TIdentVar(name) => TTopLevelIdent(name)
          case other => throw new ICE("""Error: Cannot lift a non
            |ident var""".stripMargin)
        }

        val (freeValsTuple, freeValsType) =
          insertFunctionFor(newName, Some(oldName), fun.patterns,
                            letEnv, oldName)

        // create a new valdec name:
        val valdecName = VariableGenerator.newTVariable()

        // we must add the call type to the top level environment here.
        val callType = TFunctionType(freeValsType, oldFunctionType)
        val callTypeIdent = VariableGenerator.newTVariable()

        letEnv.add(callTypeIdent, callType, false)
        letEnv.add(valdecName, oldFunctionType, false)

        // We need to update the function itself, which is the only
        // other place this could be used.
        //
        // The uses of the function are updated below.
        fun.name = newName

        // There are two sections to the update:
        //    - First, we do the update of other functions.  These will
        //      be lifted, so need to refer to the top level fundec.
        //
        //    - Second, we do the valdecs and the expression of the let.
        //      These refer to the new valdec.
        //
        //  Fist part:
        // Before we replace any recursive applications, we need to replace
        // the names with the new name.
        val nameUpdaterMap =
          new HashMap[TNamedIdent, (TNamedIdent, TType)]()
        nameUpdaterMap(oldName) = (fun.name, callType)
        // Replace any recursive applications with the curried arguments.
        val callUpdater =
          new FunCallUpdateWalk(fun.name, freeValsTuple, freeValsType,
                                oldFunctionType, env)
        // We walk the function lists in this manner.
        //
        // Note that this is OK to do as the functions are in
        // order.  So, any function that calls this functions
        // has yet to be lambda lifted.
        fundecs.foreach {
          ChangeIdentNames.newNamesFor(nameUpdaterMap, _, letEnv)
        }
        fundecs.foreach(callUpdater((), _))

        // We also have to walk the top level functions as nested
        // function declarations (that have been recursively lifted)
        // may have referenced this function.  This is an extremely inefficient
        // way of doing this, if this is too slow we can pass the previously
        // lifted functions up the tree for walking.
        newToplevelFunctions.foreach {
          ChangeIdentNames.newNamesFor(nameUpdaterMap, _, program.typeEnv)
        }
        newToplevelFunctions.foreach(callUpdater((), _))

        // Second part (replace names within the valdecs):
        val newVal =
          TVal(valdecName,
               TExpFunApp(TExpIdent(fun.name), freeValsTuple, callTypeIdent))

        // replace all uses of the function with uses of the new
        // valdec.
        val functionReplacementMap =
          new HashMap[TNamedIdent, (TNamedIdent, TType)]()
        functionReplacementMap(oldName) =
          (valdecName, oldFunctionType)

        // Update the let structure so that the changes
        // propagate back into it. (Resolving a previous bug
        // with:
        // let val x = 1; fun f () = x
        // in f end)
        //
        // Note that the ordering of addition to the valdecs is extremely
        // important here.
        //
        // Further, it is important that we do not walk the fundecs at this
        // point.
        valdecs = valdecs :+ newVal
        let.decs = valdecs
        let.exp = exp

        ChangeIdentNames.newNamesFor(functionReplacementMap, let, letEnv)

        // Now reset the letdecs:
        let.decs = valdecs ::: fundecs
      }

      // Update the vals:
      let.decs = valdecs
      None
    }
    case expFn @ TExpFn(patterns, typIdent) => {
      // Get out the old function type before it is substituted.
      val oldFunctionType = env.getNoSubstituteOrFail(typIdent)

      // Create a new function name and a new function at the top level
      // for this.
      val newName = TTopLevelIdent(FunctionNameGenerator.newAnonymousName())

      val (freeValsTuple, freeValsType) =
        insertFunctionFor(newName, None, patterns, env, typIdent)

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
   * 'newName' corresponds to the name that the function will have at
   * the top level.  'oldName' corresponds to the name the function used
   * to have.  It may be None for annonymous functions.
   * 'typeEnvName' corresponds to the name used to get to the function
   * type in the environment.
   */
  def insertFunctionFor(newName: TNamedIdent, oldName: Option[TNamedIdent],
                        patterns: List[TExpMatchRow], innerEnv: TTypeEnv,
                        typeEnvName: TIdent) = {
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
    val freeValsList =
      FreeValsWalk(program, oldName, program.typeEnv, patterns)

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
    val oldFunctionType = innerEnv.getNoSubstituteOrFail(typeEnvName)
    innerEnv.remove(typeEnvName)

    // Add to the top level environment.
    val newFunctionType = new TFunctionType(freeValsType, oldFunctionType)
    program.typeEnv.add(newName, newFunctionType, true)

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
                                 TExpFn(updatedPatterns, newName),
                                 innerEnv)

    // Actually make the new function call:
    val newFunction = TFun(newName, updatedPatterns)

    // Add this function to the top level. This is for certain parts of this
    // function that work only on the new functions.
    newToplevelFunctions = newFunction :: newToplevelFunctions
    // While this is to build up the new program.  It is also accumulated
    // here for the sake of the dec finder.
    program.funs = newFunction :: program.funs

    (freeValsExpTuple, freeValsType)
  }

  override def apply(env: TTypeEnv, p: TProgram): Unit = {
    val funsRes = p.funs.map(apply(env, _))
    val valsRes = p.vals.map(apply(env, _))

    // We do not set p.funs here because that is set by the inner
    // function using mutability.

    p.vals = getNew(p.vals, valsRes, (x: TDec) => x.asInstanceOf[TVal])
  }
}
