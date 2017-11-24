package typecheck

import frontend._
import toplev.Pass
import toplev.Shared

import exceptions._

/*
 * This pass explicitly annotates the tree with all the correct
 * types. Duplicate types are deleted.
 *
 * It uses the standard Hindley Milner algorithm.
 */

object HindleyMilner extends Pass[ASTProgram, ASTProgram]("typecheck") {
  override def treeToString(tree: ASTProgram) = """
  %s

  __ Program ___

  %s

  ___ Formatted Version ___

  %s
  """.format(tree.env.map(_.prettyPrint), tree.prettyPrint, tree.toString)

  /* This is used to store all inner environments that
   * are created. This is nessecary because the top level
   * has to be able to unify set based types.
   */
  var innerEnvs = List[ASTTypeEnv]()

  /* The top level is a special case, because some unification
   * (e.g. the conversion of ASTNumberType's to ASTIntType
   * must occur at the top level only.
   *
   * This is also an optimization against the unifiers growing indefintiely,
   * as at the top level we do not have to keep track of unifiers.
   */
  def toplevelPrincipalType(env: ASTTypeEnv,
                            decs: List[ASTDeclaration]): Unit = {
    for (dec <- decs) {
      principalType(env, dec)

      // This is done after every declaration. If this is going too slow,,
      // it could be applied to the unifier instead (which would mean
      // delaying the application of the unifier until this level).
      innerEnvs.foreach(_.specializeAtomicsMatching({
        case ASTNumberType(_) => true
        // Comparators also default to int.
        case ASTComparableType(_) => true
        case ASTIntStringType(_) => true
        case _ => false
      }, ASTIntType()))
    }
  }

  /* These functions implement the Hindley-Milner type inference algorithm.
   *
   * There is a caveat, which is that 'env' is MUTABLE and thus may not
   * be modified on something that could turn out to be a mispredict.
   */
  def principalType(env: ASTTypeEnv,
                    decs: List[ASTDeclaration]): ASTUnifier = {
    val unifier = ASTUnifier()

    for (dec <- decs) {
      unifier mguUnify principalType(env, dec)
    }

    unifier
  }

  def principalType(env: ASTTypeEnv,
                    dec: ASTDeclaration): ASTUnifier =
    dec match {
      case ASTValBind(lhs, rhs) => {
        // Note that the unifier is not used here.
        val (unifier, typ) = principalType(env, rhs)

        // Before inserting the types as found, we must ensure that
        // typ is of the correct approximate type to match. Consider;
        // let val (a, b) = f x ....
        //
        // Where f is 'a -> 'b. Then, we need to change (this instance of)
        // the 'b into ('c, 'd) to match with the tuple type.
        def genAdjustedTyp(idents: ASTIdent, typ: ASTType): ASTType =
          idents match {
            case ASTIdentTuple(exp :: Nil) => typ
            case ASTIdentTuple(exps) => {
              val resType =
                ASTTupleType(exps.map(typ =>
                    genAdjustedTyp(typ, TypeVariableGenerator.getVar())))
              unifier mguUnify (typ unify resType)
              resType
            }
            // Adding list types would require additions here.
            case other => typ
          }

        val adjustedTyp = genAdjustedTyp(lhs, typ)
        unifier mguUnify (typ unify adjustedTyp)

        // We update the environment with the new types
        // as appropriate.
        insertTypes(env, lhs, adjustedTyp)
        unifier.apply(env)
        unifier
      }
      case fun @ ASTFunBind(cases) => {
        // This case is complicated by the presence of multiple
        // cases. In addition to HindleyMilner type checking,
        // we must check that all the cases make logical sense.
        assert(cases.length > 0)

        val idents: List[ASTIdent] = cases map (_._1)

        // Require that all the idents are string identifiers, not
        // some symbols:
        if (idents.exists(x => !x.isInstanceOf[ASTIdentVar])) {
          throw new InferenceException("The function identifiers " +
            idents.toString + " are not valid")
        }

        // We also require that all the identifiers are the same
        idents.foldLeft(idents(0)) {
          case (ASTIdentVar(idName), ASTIdentVar(prevName)) =>
            if (idName != prevName) {
              throw new InferenceException("""
                |Expected all function identifiers in a single  function
                |declaration to have the same name. Instead, found
                |"%s" in a block with "%s"""".stripMargin.format(idName,
                                                                 prevName))
            } else
              ASTIdentVar(idName)
          case _ => unreachable
        }

        // Now, we first type the pattern for each, before the
        // expression.
        val patterns = cases map(_._2)
        val types = cases map (_._3)
        val exprs = cases map (_._4)

        // Since functions may be recursive, insert a template for this
        // function (NOT qualified so it may be resolved by a unifier)
        env.add(idents(0), ASTFunctionType(TypeVariableGenerator.getVar(),
                                           TypeVariableGenerator.getVar()),
                false)

        // The pattern matching function does not return a unifier.
        // That is done internally. Each pattern row gets an associated
        // environment (because they are all different)
        val (funType, resultEnvs, unifier) =
          listPatternType(patterns, types, exprs, env)

        val functionTypeUnifier = funType unify env.getOrFail(idents(0))
        // Finally, re-add this function (forall qualified) to the
        // global environment
        env.updateId(idents(0),
                     functionTypeUnifier(env.getOrFail(idents(0))), true)

        // We need to apply the unifier as the function definition may
        // have modified a non-polymorphic type.
        unifier.apply(env)

        // We finally set the row environments so that we don't loose those
        fun.rowEnvs = Some(resultEnvs)
        unifier
      }
      case ASTDataType(ident, astDatatConstructors) => {
        println("""Datatypes are not currently supported. """)
        System.exit(1)
        unreachable
      }
    }

  def principalType(env: ASTTypeEnv, expr: ASTExp): (ASTUnifier, ASTType) =
    expr match {
      case ASTExpConst(const) => (ASTUnifier(), const.getType)
      case ASTExpIdent(ident) => {
        val foundIdent = env.get(ident) match {
          case None => throw new UndeclaredIdentException("""
            Error: Identifier %s is not defined. """.format(ident.prettyPrint))
          case Some(typ) => typ
        }

        (ASTUnifier(), foundIdent)
      }
      case application @ ASTExpFunApp(fun, app) => {
        val (funUnifier, declaredFunType) = principalType(env, fun)
        val (appUnifier, appType) = principalType(env, app)

        val resultType = TypeVariableGenerator.getVar()

        val mgu = ASTType.unify(declaredFunType,
                                ASTFunctionType(appType, resultType))

        // This is used for later optimizations. It is helpful
        // to know exactly what types the function is going
        // to be called with for specialization.
        //
        // This is a safe case as the unification of a fun type with
        // something must be a fun type
        val callTypeVariable = VariableGenerator.newVariable()
        application.callType = Some(callTypeVariable)

        // Then, so that the type of the call can actually be accessed,
        // add it to the environment. We do not want this type to
        // be forall quantified so that it can be changed by
        // future unifier applications.
        env.add(callTypeVariable,
                mgu(declaredFunType).asInstanceOf[ASTFunctionType],
                false)
        // The actual result type can be given by extracting it from
        // the funTyp
        mgu mguUnify funUnifier
        mgu mguUnify appUnifier

        // Then, we also require that any specializations enforced
        // by the application are added in.
        // This method is carefully designed so that it does
        // not throw away the forall quantifiers around function
        // types.
        // mgu.functionSpecialze(declaredFunType, funTyp)

        (mgu, mgu.apply(resultType))
      }
      case infixApp @ ASTExpInfixApp(fun, op1, op2) => {
        // In this case, the typing is the same as a normal
        // function application. Treat it as such.
        // These will be converted later in the compilation anyway.
        val prefixFunction =
          ASTExpFunApp(ASTExpIdent(fun), ASTExpTuple(List(op1, op2)))

        val result =
          principalType(env, prefixFunction)

        infixApp.callType = prefixFunction.callType
        result
      }
      case unapp @ ASTExpUnOpApply(fun, op) => {
        // The same logic for the infix app may be applied in this case.
        val function = ASTExpFunApp(ASTExpIdent(fun), op)

        val result = principalType(env, function)

        unapp.callType = function.callType
        result
      }
      case ASTExpTuple(elems) => {
        // These may require unification, e.g. in
        //    fun f x = (not(x), x + 1)
        // but the order is not specified. The unification
        // may be done at the end.
        val unifiers = (elems map (x => principalType(env, x)))

        val resType = unifiers.foldRight (List[ASTType]()) ({
          case ((_, typ), rest) => typ :: rest
        })

        // The MGU is calculated as a return value, but it is not applied
        // to the environment. The application to the environment is
        // left as a final step.
        val mgu = ASTUnifier()
        unifiers.foreach( {
          case (unifier, _) => mgu mguUnify unifier
        })

        // In this case, it does not matter whether we apply the unifier here
        // or at the top level.
        (mgu, ASTTupleType(resType))
      }
      case ASTExpList(elems) => {
        // This used to be typed by using the other case.  However, that leads
        // to Stackoverflow errors.  Instead, we attempt to give each element
        // a type, then unify those types.
        val mgu = ASTUnifier()
        var listTyp: ASTType = TypeVariableGenerator.getVar()

        for (elem <- elems) {
          val (unifier, typ) = principalType(env, elem)

          mgu mguUnify unifier
          mgu mguUnify (listTyp unify typ)

          listTyp = mgu(listTyp)
        }

        (mgu, ASTListType(listTyp))
      }
      case letStmt @ ASTExpLetIn(decs, exp) => {
        // Let-In is an example of a type that requires the construction
        // of new typing environment.
        val letEnv = new ASTTypeEnv(Some(env))
        innerEnvs = letEnv :: innerEnvs

        // Set the environment of the let env.
        // This may be done here as the environment is mutable
        // and so will be updated.
        letStmt.typeEnv = Some(letEnv)

        // Type the declarations:
        val decsUnifier = principalType(letEnv, decs)

        // Then return the type of the let:
        // We intentionally  return the toplevel environment
        // since the let local environment is not relevant
        // beyond this let.
        val (unifier, typ) = principalType(letEnv, ASTExpSeq(exp))

        unifier mguUnify decsUnifier
        (unifier, typ)
      }
      case ASTExpSeq(exps) => {
        assert(exps.length > 0)
        // Note that the type of this is given by the last type.
        val types = exps.map((x) => principalType(env, x))

        val unifier = frontend.ASTUnifier()
        types.foreach {
          case (nextUnifier, _) => unifier mguUnify nextUnifier
        }

        // Note that the type is given by the last element of
        // exps
        (unifier, types.last._2)
      }
      case ASTExpTyped(exp, typ) => {
        val (unifier, inferedTyp) = principalType(env, exp)

        // Since we have chosen to use Ocaml semantics here,
        // we may 'unify' rather the specialize these types.
        // Note that the ordering is important here.  It can
        // lead to cycles in the unifier if we unify the infered
        // typ to the typ.
        unifier mguUnify unifier.unifyTo(typ, inferedTyp)

        (unifier, typ)
      }
      case ifThenElse @ ASTExpIfThenElse(cond, ifTrue, ifFalse) => {
        val (condUnifier, condType) = principalType(env, cond)
        condUnifier.specializeVerify(condType, ASTBoolType())
        condUnifier(env)

        val (trueUnifier, trueType) = principalType(env, ifTrue)
        trueUnifier(env)

        val (falseUnifier, falseType) = principalType(env, ifFalse)
        falseUnifier(env)


        val mgu = condUnifier
        mgu.mguUnify(trueUnifier)
        mgu.mguUnify(falseUnifier)

        val casesUnifier = mgu(trueType).unify(mgu(falseType))
        mgu.mguUnify(casesUnifier)

        if (mgu(trueType) != mgu(falseType))
          throw new ICE("""Types %s and %s should have unified
            but failed to""".format(trueType, falseType))

        // We need to add the type of the result to the environment.
        val branchTypeID = VariableGenerator.newVariable()
        env.add(branchTypeID, mgu(trueType), false)
        ifThenElse.branchType = Some(branchTypeID)

        (mgu, mgu(trueType))
      }
      case caseStmt @ ASTExpCase(cond, cases) => {
        // This is approachable as an annonymous function typing
        // and a fun-app. (Note that this is a reduction in the SML-97
        // spec.)
        val fnDec = ASTExpFunApp(ASTExpFn(cases), cond)
        val (unifier, typ) = principalType(env, fnDec)

        // We need to set up the correct case type in the statement:
        caseStmt.applicationType = fnDec.callType
        (unifier, typ)
      }
      case matchRow @ ASTExpMatchRow(pat, exp) =>
        // This should never be called. It needs to be handled specially
        // in the ASTExpFn or ASTExpCase cases (the typer needs
        // to see ALL the rows at once, not just a single row)
        unreachable
      case fn @ ASTExpFn(body) => {
        // This is very similar to the function type. We do the same
        // thing here:

        // Body is a list of ASTExpMatchRows(ASTPat, ASTExp):
        val patterns = body map { case ASTExpMatchRow(pat, _) => pat }
        val exprs = body map { case ASTExpMatchRow(_, expr) => expr }
        assert(patterns.length == exprs.length)

        val types: List[Option[ASTType]] =
          (for (i <- 0 until patterns.length) yield None).toList

        val (typ, patternEnvs, unifier) =
          listPatternType(patterns, types, exprs, env)

        // Set the environments in each of the rows.
        (body, patternEnvs).zipped.foreach {
          case (matchRow, env) => matchRow.env = Some(env)
        }

        // Add that type to the environment under a new name.
        val funTypeIdentifier = VariableGenerator.newVariable()

        env.add(funTypeIdentifier,
                // For safety, we assert here that this is indeed
                // a function type.
                unifier(typ).asInstanceOf[ASTFunctionType],
                false)

        // We need to retain a reference to the identifer:
        fn.funType = Some(funTypeIdentifier)

        (unifier, typ)
      }
    }


  def listPatternType(patterns: List[List[ASTPat]],
                      types: List[Option[ASTType]],
                      exprs: List[ASTExp],
                      parentEnv: ASTTypeEnv): (ASTType, List[ASTTypeEnv],
                                               ASTUnifier) = {
    assert(patterns.length > 0)
    assert(patterns.length == types.length)
    assert(types.length == exprs.length)
    val expUnifier = ASTUnifier()
    val patUnifier = (for (x <- 0 until patterns(0).length)
                        yield ASTUnifier()).toList
    val rowEnvs = (for (x <- 0 until patterns.length)
                    yield new ASTTypeEnv(Some(parentEnv))).toList

    var argType: List[ASTType] = (for (x <- 0 until patterns(0).length)
                              yield TypeVariableGenerator.getVar()).toList
    var resultType: ASTType = TypeVariableGenerator.getVar()

    innerEnvs = rowEnvs ::: innerEnvs

    // Verify that each of the pattern lists are the same lengths:
    if (!patterns.forall(x => x.length == patterns(0).length)) {
      throw new BadPatternException("""
        |The patterns:
        |%s
        |Do are not all the same length""".stripMargin.format(
          patterns.map(_.map(_.prettyPrint).mkString(" ")).mkString("\n")))
    }

    for (i <- 0 until patterns.length) {
      val pattern = patterns(i)
      val exp = exprs(i)
      val typ = types(i)
      val rowEnv = rowEnvs(i)

      val (thisPatType, thisPatUnifier) = setupPatEnv(rowEnv, pattern)

      // Apply the correct types from the  pattern unifier.
      (patUnifier zip thisPatUnifier).map({ case (x, y) => x mguUnify y })

      val (thisExpUnifier, expType) = principalType(rowEnv, exp)
      expUnifier mguUnify thisExpUnifier

      typ match {
        case Some(typ) => expUnifier mguUnify (expType.unify(typ))
        case None => // Do nothing.
      }

      // Finally, compute the unification of the result types
      // to ensure that those are all the same.
      // And that any assigned types in the pattern propagate through.
      val resultTypeUnifier = resultType unify (expUnifier(expType))
      resultTypeUnifier mguUnifyAll patUnifier

      resultType = resultTypeUnifier(resultType)
      expUnifier mguUnify resultTypeUnifier

      // We must also unify the expression unifier with the pattern
      // unifiers.
      patUnifier.foreach(_ mguUnify expUnifier)

      val argTypeUnifier = ASTUnifier()
      // Also need to compute the arguement types to ensure
      // that they are all the same.
      (argType, thisPatType).zipped.foreach {
        case (argType, patType) =>
          argTypeUnifier mguUnify (argType unify(patType))
      }

      argTypeUnifier mguUnify expUnifier

      argTypeUnifier.mguUnifyAll(patUnifier)
      patUnifier.foreach(_.mguUnify(argTypeUnifier))
      argType = argType.map(argTypeUnifier(_))

      // Finally, apply this to the row environment
      patUnifier.foreach(_.apply(rowEnv))
    }

    // We do this with the original list because we want
    // the bracketing to be with the first element as the
    // 'most outside' element. This effectively reverses the list.
    val functionType =
      argType.foldRight(resultType) {
        case (typ, buildUp) => ASTFunctionType(typ, buildUp)
    }

    // We may just return the expUnifier as all variables created
    // in patterns are fresh so cannot affect state outside of this
    // function.
    (functionType, rowEnvs, expUnifier)
  }

  /* ASTPat's do not only have classical principal types.
   *
   * Instead, they are used to create environments that correctly
   * represent the context they are in.
   *
   * This is not done by the principalType algorithm because it must
   * correlate between multiple rows.
   */
  def setupPatEnv(env: ASTTypeEnv, pat: List[ASTPat]):
      (List[ASTType], List[ASTUnifier]) = {
    // These represent the return values BACKWARDS.
    // They are reversed before returning (used backwards
    // so that the '::' method can be used.
    var astTypes = List[ASTType]()
    var astUnifiers = List[ASTUnifier]()

    for (patItem <- pat) {
      patItem match {
        case ASTPatWildcard(typs) => {
          val defaultType = TypeVariableGenerator.getVar()
          val unifier = unifyTypeList(defaultType :: typs)

          astTypes = unifier(defaultType) :: astTypes
          astUnifiers = unifier :: astUnifiers
        }
        case ASTPatVariable(variable, typs) => variable match {
          case ASTIdentVar(name) => {
            val defaultGenericType = TypeVariableGenerator.getVar()
            val resUnifier = unifyTypeList(defaultGenericType :: typs)

            astTypes = resUnifier(defaultGenericType) :: astTypes
            astUnifiers = resUnifier :: astUnifiers

            if (env.innermostHasType(variable))
              throw new BadPatternException("""Error, there are duplicate
                varaibles in the pattern: %s""".format(patItem.prettyPrint))
            else
              env.add(variable, resUnifier(defaultGenericType), false)
          }
          case ASTEmptyListIdent() => {
            val emptyListType = ASTListType(TypeVariableGenerator.getVar())
            val resUnifier = unifyTypeList(emptyListType :: typs)

            astTypes = resUnifier(emptyListType) :: astTypes
            astUnifiers = resUnifier :: astUnifiers
          }
          case ASTUnitIdent() => {
            val resUnifier = unifyTypeList(ASTUnitType() :: typs)

            astTypes = ASTUnitType() :: astTypes
            astUnifiers = resUnifier :: astUnifiers
          }
          case other =>
            throw new ICE("""Error, ident type %s is not expected
              |in a pattern""".stripMargin.format(variable.prettyPrint))
        }
        case ASTPatSeq(seq, typs) => {
          // Note that there are no duplicate variable names
          // allowed within a single pattern entry.
          val duplicateSet = new ASTTypeSet()
          val unifier = new ASTUnifier()

          // This is also stored backwards and reversed
          // at the last step
          var seqTypes = List[ASTType]()

          for (seqElement <- seq) {
            val (patType, patUnifiers) = setupPatEnv(env, List(seqElement))

            assert(patType.length == 1)
            assert(patUnifiers.length == 1)

            // The unifier is just unified together.
            unifier mguUnifyAll(patUnifiers)

            seqTypes = patType(0) :: seqTypes
          }

          // The specified types must be used to constrain the actual
          // pattern.
          val genericTypeHead = TypeVariableGenerator.getVar()
          val typListUnifier = unifyTypeList(genericTypeHead :: typs)
          val constrainedTyp = typListUnifier(genericTypeHead)
          val typUnifier =
            if(seqTypes.length == 1)
              constrainedTyp unify (seqTypes(0))
            else
              constrainedTyp unify (ASTTupleType(seqTypes.reverse))
          unifier mguUnify typUnifier
          unifier mguUnify typListUnifier

          // The types are combined into an ASTTupleType.
          astTypes =
            if (seqTypes.length == 1)
              unifier(seqTypes(0)) :: astTypes
            else
              unifier(ASTTupleType(seqTypes.reverse)) :: astTypes
          astUnifiers = unifier :: astUnifiers
        }
        case ASTListPat(list, typ) => {
          // Again, there are no duplicate variable names allowed
          // within a single pattern entry.
          val duplicateSet = new ASTTypeSet()
          val unifier = new ASTUnifier()
          var listType: ASTType = TypeVariableGenerator.getVar()

          for (listElement <- list) {
            // This is very similar to the above case.
            val (elemTyp, elemUnifiers) = setupPatEnv(env, List(listElement))
            assert(elemUnifiers.length == 1)
            assert(elemTyp.length == 1)

            // It is not possible to have two types in a list
            // element (note that tuples are considered a single
            // element)
            unifier mguUnify(elemUnifiers(0))

            // The difference is with the unification, which must be
            // done to a single type.
            val thisElemUnifier = listType unify elemTyp(0)
            unifier mguUnify thisElemUnifier
            listType = thisElemUnifier(listType)
          }

          // Finally, we must ensure that the type seen corresponds to
          // the type specified.
          val genericListType = ASTListType(TypeVariableGenerator.getVar())
          val typListUnifier = unifyTypeList(genericListType :: typ)
          val specifiedType = typListUnifier(genericListType)
          val specifiedUnifier = ASTListType(listType) unify specifiedType
          unifier mguUnify specifiedUnifier
          unifier mguUnify typListUnifier

          unifier(env)

          astUnifiers = unifier :: astUnifiers
          astTypes = unifier(specifiedType) :: astTypes

        }
        case ASTPatConst(ident, typ) => {
          // Reals are not a valid pattern identifier.
          ident match {
            case ASTConstFloat(_) => throw new BadPatternException(
              "Real %s in pattern".format(ident.prettyPrint))
            case _ => {
              val genericTyp = TypeVariableGenerator.getVar()
              val typeListUnifier = unifyTypeList(genericTyp :: typ)
              val specifiedType = typeListUnifier(genericTyp)
              val constTyp = ident.getType

              val unifier = specifiedType unify constTyp
              typeListUnifier mguUnify unifier

              astUnifiers = typeListUnifier :: astUnifiers
              astTypes = constTyp :: astTypes
            }
          }
        }
        case ASTPatCons(head, tail) => {
          val (headTyps, headUnifiers) = setupPatEnv(env, List(head))
          val (tailTyps, tailUnifiers) = setupPatEnv(env, List(tail))

          assert(headTyps.length == 1)
          assert(tailTyps.length == 1)
          assert(headUnifiers.length == 1)
          assert(tailUnifiers.length == 1)

          val mgu = ASTUnifier()

          mgu mguUnify headUnifiers(0)
          mgu mguUnify tailUnifiers(0)

          val headTailUnifier = ASTListType(headTyps(0)) unify tailTyps(0)

          mgu mguUnify headTailUnifier

          mgu(env)

          astTypes = mgu(tailTyps(0)) :: astTypes
          astUnifiers = mgu :: astUnifiers
        }
      }
    }

    // We reverse the ASTTypes to avoid quadratic time in the number
    // of arguments.
    (astTypes.reverse, astUnifiers.reverse)
  }

  /* This function takes a list of types and returns a single unifier
   * that represents the entire list if that is possible. If that
   * is not possible, it throws an exception.
   *
   * It is expected that the list is non-empty.
   */
  def unifyTypeList(typList: List[ASTType]): ASTUnifier = {
    assert(typList.length > 0)

    val givenTypeUnifier = ASTUnifier()
    typList.tail.foldLeft (typList.head) {
      case (lastTyp, thisTyp) => {
        // Unify the two types, apply that and pass the newly
        // unified type on.
        val unifier =
          givenTypeUnifier(lastTyp).unify(givenTypeUnifier(thisTyp))
        givenTypeUnifier.mguUnify(unifier)
        thisTyp
      }
    }

    // Lastly, apply the unifier to the fist type specified.
    givenTypeUnifier
  }

  /* This function takes a list of names (as would be delcared
   * in a tuple) and checks that the types line up. It then inserts
   * them into the environment.
   *
   * Note: If this compiler is changed to support arbitrary patterns
   * as l-values this function will need to be beefed out. (This is
   * where typechecking should occur for things like:
   *
   *    val [x] = [];
   *
   * The current algorithm is  a recursive descent that ensures
   * that both sides have the same type.
   */
  def insertTypes(env: ASTTypeEnv, names: ASTIdent,
                  typs: ASTType): Unit = (names, typs) match {
    case (ASTIdentTuple(Nil), _) => unreachable
    // Note that '_' is a special identifier.
    // We do not add that.
    case (ASTIdentTuple(ident :: Nil), typ) => {
      env.add(ident, typ, true)
    }
    case (ASTIdentTuple(idents), ASTTupleType(typList)) =>
      // We note that each one of these names could be an ASTIdentTuple,
      // so we repeat the process.
      if (idents.length != typList.length)
        throw new InferenceException("""Could not unify l-values %s
          and r-values %s"""format(names.prettyPrint, typs.prettyPrint))
      else
        (typList zip idents).foreach({
          case (typ, ASTIdentTuple(name)) =>
                  insertTypes(env, ASTIdentTuple(name), typ)
          case (typ, name) =>
                  env.add(name, typ, true)
        })
    case (ASTIdentTuple(idents), typ) =>
      // This case is reached in the case that the identifier is a tuple,
      // but the RHS was given some non-tuple type.
      throw new InferenceException("""Could not unify l-values %s
        with type %s""".format(names.prettyPrint, typ.prettyPrint))
    case (name, typ) =>
      env.add(name, typ, true)
  }

  def run(tree: ASTProgram) = {
    // This is executed as a sequential process.
    try {
      val env = new ASTTypeEnv()
      innerEnvs = env :: innerEnvs

      toplevelPrincipalType(env, tree.decs)

      tree.env = Some(env)
      tree
    } catch {
      case e: InferenceException => {
        println("Type inference error: ")
        println(e.getMessage())
        if (Shared.debug) {
          e.printStackTrace()
        }
        System.exit(1)
        unreachable
      }
      case e: UndeclaredIdentException => {
        println("Unrecognized Identifier")
        println(e.getMessage())
        if (Shared.debug) {
          e.printStackTrace()
        }
        System.exit(1)
        unreachable
      }
      case e: SpecializationError => {
        println("Error specializing: ")
        println(e.getMessage())
        if (Shared.debug) {
          e.printStackTrace()
        }
        System.exit(1)
        unreachable
      }
      case e: UnificationError => {
        println("Error Unifying:")
        println(e.getMessage())
        if (Shared.debug) {
          e.printStackTrace()
        }
        System.exit(1)
        unreachable
      }
      case e: BadPatternException => {
        println("Bad Pattern:")
        println(e.getMessage())
        if (Shared.debug) {
          e.printStackTrace()
        }
        System.exit(1)
        unreachable
      }
    }
  }
}
