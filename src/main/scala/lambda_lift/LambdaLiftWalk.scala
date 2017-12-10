package lambda_lift

import ast_change_names.FunctionNameGenerator
import change_names.ChangeIdentNames
import exceptions.ICE
import scala.collection.mutable.{HashMap,HashSet}
import tir._
import tpass.TTypeEnvUpdateParentPass
import typecheck.VariableGenerator

class LambdaLiftWalk(val program: TProgram)
    extends TTypeEnvUpdateParentPass {
  var newTopLevelFunctions = List[TFun]()

  // This is to keep the list of variables that should be uniqueified once
  // this pass is finished.
  //
  // They cannot be uniqueified during this pass because we need to be
  // able to look through previously lambda lifted functions to check
  // for free variables.  That is:
  //
  // val z = 10
  // fun f x = let fun g y = f y + 1 in z end
  //
  // We need 'g' to be passed 'z' (so that it can pass it on to 'f')
  // and so names cannot be uniqueified until later.
  var introducedVariables =
    new HashMap[TNamedIdent, (List[TNamedIdent], List[TType])]()

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case let @ TExpLetIn(decs, _, letEnv) => {
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

      // Finally, walk the exp:
      val letInResult = apply(letEnv, let.exp)
      let.exp = getNew(let.exp, letInResult)

      // For each function, find the free variables and create a lambda lifted
      // function that takes those variables as parameters.
      //
      // Add that as a valdec to the list of valdecs with the closure
      // arguments applied to it.
      for (dec <- decs) {
        // Check if any VALs are actually just FnDecs. Do this by first walking
        // the vals with this (so to extract the annon function definitions).
        //
        // Then check that each val has RHS that is a function
        // identifier.
        //
        // Replace that in the body.
        decs.foreach(apply(letEnv, _))

        dec match {
          case fun @ TFun(_, _) => {
            val oldFunctionType = letEnv.getNoSubstituteOrFail(fun.name)
            val oldName = fun.name
            // Then set the function to have a new top level name
            // and update the uses of that function:
            val newName = fun.name match {
              case TIdentVar(name) => TTopLevelIdent(name)
              case other => throw new ICE("""Error: Cannot lift a non
                |ident var""".stripMargin)
            }

            insertFunctionFor(newName, Some(oldName), fun.patterns,
                              letEnv, oldName) match {
              case Some((freeValsTuple, freeValsType)) => {
                // create a new valdec name:
                val valdecName = VariableGenerator.newTVariable()

                // we must add the call type to the top level environment here.
                val callType = TFunctionType(freeValsType, oldFunctionType)
                val callTypeIdent = VariableGenerator.newTInternalVariable()

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
                //    - Second, we do the valdecs and the expression of the
                //      let.  These refer to the new valdec.
                //
                //  Fist part:
                // Before we replace any recursive applications, we need to
                // replace the names with the new name.
                val nameUpdaterMap =
                  new HashMap[TNamedIdent, (TNamedIdent, TType)]()
                nameUpdaterMap(oldName) = (fun.name, callType)
                // Replace any recursive applications with the curried
                // arguments.
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
                // may have referenced this function.  This is an extremely
                // inefficient way of doing this, if this is too slow we can
                // pass the previously lifted functions up the tree for
                // walking.
                newTopLevelFunctions.foreach {
                  ChangeIdentNames.newNamesFor(nameUpdaterMap, _,
                                               program.typeEnv)
                }
                newTopLevelFunctions.foreach(callUpdater((), _))

                // Second part (replace names within the valdecs):
                val newVal =
                  TVal(valdecName,
                       TExpFunApp(TExpIdent(fun.name),
                                  freeValsTuple.nodeClone, callTypeIdent))

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
                // Note that the ordering of addition to the valdecs is
                // extremely important here.  The idea is that we should insert
                // the new function at the first  point after all the
                // prerequisites for the function have been declared.  This can
                // be done by keeping track of the variables we need and
                // removing them as we come across the decs.
                val freeIdentsList = freeValsTuple match {
                  case TExpTuple(elems) => elems map {
                    case TExpIdent(ident: TNamedIdent) => ident
                    case _ => throw new ICE("Expected a TNamedIdent")
                  }
                  case TExpIdent(ident: TNamedIdent) => List(ident)
                  case other => throw new ICE("""Expected a TNamedIdent,
                    |instead found %s""".stripMargin.format(other.prettyPrint))
                }
                valdecs = insertIntoList(valdecs, newVal, freeIdentsList)

                //
                // Further, it is important that we do not walk the fundecs at
                // this point.
                val tempLet = TExpLetIn(valdecs, let.exp, letEnv)

                ChangeIdentNames.newNamesFor(functionReplacementMap, let,
                                             letEnv)
              }
              case None => {
                // In this case, there were no free variables to introduce,
                // so we can just change the names of the references
                // to the function.
                val functionReplacementMap =
                  new HashMap[TNamedIdent, (TNamedIdent, TType)]()
                functionReplacementMap(oldName) = (newName, oldFunctionType)

                ChangeIdentNames.newNamesFor(functionReplacementMap, let,
                                             letEnv)
              }
            }
          }
          case TVal(_, _) => // Do nothing
          case TJavaFun(_, _, _, _) =>
            throw new ICE("""TJavaFun encountered before they are allowed to be
              |introduced""".stripMargin)
        }
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

      insertFunctionFor(newName, None, patterns, env, typIdent) match {
        case Some((freeValsTuple, freeValsType)) => {
          // Create a new type variable reference for the introduced function
          // application.
          val applicationIdent = VariableGenerator.newTInternalVariable()

          env.add(applicationIdent,
                  TFunctionType(freeValsType, oldFunctionType), false)

          // The only use of this funciton is where it is declared.
          // Update that use.
          Some(TExpFunApp(TExpIdent(newName), freeValsTuple, applicationIdent))
        }
        case None => {
          // There were no free variables so the function call does not
          // have to be added. Simply return a reference to the new name.
          Some(TExpIdent(newName))
        }
      }
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

    // Remove the old function type
    val oldFunctionType = innerEnv.getNoSubstituteOrFail(typeEnvName)
    innerEnv.remove(typeEnvName)

    if (freeValsList.length > 0) {
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
      val freeCount = freeValsList.length

      val freeValsPatList = freeValsNamesList.map(new TPatVariable(_))
      val freeValsType = if (freeCount > 1) new TTupleType(freeValsTypesList)
                         else freeValsTypesList(0)
      val freeValsPatTuple = if (freeCount > 1) new TPatSeq(freeValsPatList)
                             else freeValsPatList(0)
      val freeValsExpTuple =
        if (freeCount > 1)
          new TExpTuple(freeValsNamesList.map(new TExpIdent(_)))
        else
          new TExpIdent(freeValsNamesList(0))

      // Add these new variables and types (that will need to be uniqueified)
      // to the map for uniqueification:
      introducedVariables(newName) = (freeValsNamesList, freeValsTypesList)

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
      // Actually make the new function call:
      val newFunction = TFun(newName, updatedPatterns)

      // Add this function to the top level. This is for certain parts of this
      // function that work only on the new functions.
      newTopLevelFunctions = newFunction :: newTopLevelFunctions
      // While this is to build up the new program.  It is also accumulated
      // here for the sake of the dec finder.
      program.funs = newFunction :: program.funs

      Some(freeValsExpTuple, freeValsType)
    } else {
      // Add to the top level environment.
      program.typeEnv.add(newName, oldFunctionType, true)

      // Insert some empty lists into the introduced variables
      // set since there were no variables introduced.
      introducedVariables(newName) = (List(), List())

      // There are no free variables in the function, so we can just
      // add it straight to the top level.
      val newFunction = TFun(newName, patterns)
      newTopLevelFunctions = newFunction :: newTopLevelFunctions
      program.funs = newFunction :: program.funs

      None
    }
  }

  /* This is a backwards walk on the valdecs list.
   *
   * Start by walking the list from the back.  When we run into a
   * variable needed in the environment of newVal, then we stop and
   * insert the new val there.
   */
  def insertIntoList(valdecs: List[TVal], newVal: TVal,
                     freeVariables: List[TNamedIdent]) = {
    val freeVariablesSet = new HashSet[TNamedIdent]()
    freeVariables.foreach(freeVariablesSet.+=(_))
    var front = List[TVal]()
    var tail = valdecs.reverse

    val (before, after) = tail.span {
      _.ident.getDeclaredIdents.forall(!freeVariablesSet.contains(_))
    }

    // Then rebuild the list:
    (before ++ List(newVal) ++ after).reverse
  }

  override def apply(env: TTypeEnv, p: TProgram): Unit = {
    val funsRes = p.funs.map(apply(env, _))
    val valsRes = p.vals.map(apply(env, _))

    // We do not set p.funs here because that is set by the inner
    // function using mutability.

    p.vals = getNew(p.vals, valsRes, (x: TDec) => x.asInstanceOf[TVal])
  }
}
