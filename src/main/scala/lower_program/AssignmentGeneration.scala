package lower_program

import exceptions.ICE
import generators.VariableGenerator
import tir._

/* This is an object that provides helper methods for lowering
 * the program.
 */
object AssignmentGeneration {
  def convertToAssignNodeDec(dec: TDec,
                             typeEnv: TTypeEnv): (TExp, List[TNamedIdent]) =
    dec match {
      case TVal(ident, exp) => {
        // The idea in this case is to assign the whole expression
        // to some variable, then extract it peice by peice.
        val tempIdent = VariableGenerator.newTVariable(TValClass())

        typeEnv.add(tempIdent, typeEnv.compoundTypeOf(ident), false)

        val (assignExp, ids) =
          convertToAssignNodeIdent(tempIdent, ident, typeEnv)

        // If exp is an exp sequence, then we can put all but the last item
        // of the sequence before the assign to reduce the nesting.
        // So, instead of:
        //    Assign x to (Assign y to 10; y)
        //
        // We can generate:
        //
        //    Assign y to 10;
        //    Assign x to y
        //
        // This greately reduces the complexity of the generated code.
        // (In the general case).
        val (valueExp, beforeExp) = exp match {
          case TExpSeq(seq) => (seq.last, seq.dropRight(1))
          case other => (other, List[TExp]())
        }

        (TExpSeq(beforeExp :::
                 List(TExpAssign(tempIdent, valueExp), assignExp)).flatten,
         tempIdent :: ids)
      }
      case _ => throw new ICE("""Error: Cannot convert a TFun or a TJavaFun
        into an assign.  Statement was: %s""".format(dec.prettyPrint))
    }

  def convertToAssignNodeIdent(parentIdent: TIdent, declaration: TIdent,
                               typeEnv: TTypeEnv): (TExp, List[TNamedIdent]) =
    declaration match {
      case decIdent : TNamedIdent => {
        // If the identifier is a Top level ident, then it must not be added
        // to the list of identifiers
        val localIdents = decIdent match {
          case TTopLevelIdent(name, identClass) => List[TNamedIdent]()
          case ident: TNamedIdent => List(ident)
          case other => throw new ICE("""Error: Unexpected identifier
            |%s""".stripMargin.format(other.prettyPrint))
        }

        (TExpSeq(List(TExpAssign(decIdent, TExpIdent(parentIdent)),
                      TExpConst(TConstTrue()))),
         localIdents)
      }
      // If the tuple is of length 1, then get rid of it here.
      case TIdentTuple(subIdents) =>
        unpackList(subIdents, (index => {
                     val itemType = typeEnv.compoundTypeOf(subIdents(index))
                     val id = VariableGenerator.newTInternalVariable()
                     typeEnv.addTopLevel(id, itemType, false)
                     (TExpTupleExtract(TExpIdent(parentIdent),
                                      subIdents.length, index, id),
                      itemType)
                   }),
                   convertToAssignNodeIdent, typeEnv)
      case TUnderscoreIdent() => (TExpConst(TConstTrue()), List())
      case TUnitIdent() => (TExpConst(TConstTrue()), List())
      case _ => throw new ICE("""Error, identifier %s was not expected
        |in a pattern match""".stripMargin.format(declaration.prettyPrint))
    }

  /* Given some parent identifier from which to extract
   * the content, and some pattern, this function computes
   * a list of expressions that should be used to extract
   * a pattern
   */
  def convertToAssignNodePat(parentIdent: TIdent, pat: TPat,
                             typeEnv: TTypeEnv): (TExp, List[TNamedIdent]) =
    pat match {
      case TPatVariable(identVar) =>
        (TExpSeq(List(TExpAssign(identVar, TExpIdent(parentIdent)),
                      TExpConst(TConstTrue()))).flatten,
         List(identVar))
      case TPatIdentifier(_: TNamedIdent) => throw new ICE("""Error: A
        |TNamedIdent in a TPatIdentifier should not occur""".stripMargin)
      case TPatIdentifier(id) => id match {
        // It is safe to simply return true when matching unit
        // as there is no other value that can pass typechecking
        // and match this type.
        case TUnitIdent() => (TExpConst(TConstTrue()), List())
        // Offload this to a conversion with a list match that is
        // empty.
        case TEmptyListIdent() =>
          convertToAssignNodePat(parentIdent, TListPat(List()), typeEnv)
        case _ =>
          throw new ICE("Unpexected TPatIdentifier %s".format(id.prettyPrint))
      }
      case seq @ TPatSeq(elems) => {
        val elemsTypesList = typeEnv.getOrFail(parentIdent) match {
          case tuple @ TTupleType(typs) =>
            // This check is required because the typechecker removes
            // excess tuples around elements.  So, ((((1)))) becomes
            // 'int', which is correct, but it means that this recursion
            // breaks (because the first descent gets the type 'int' and
            // all subsequent descents are broken).  The solution to the
            // problem is to just return the list of the type if the pat
            // seq was a singleton seq.
            if (elems.length == 1)
              List(tuple)
            else
              typs
          case other =>
            if (elems.length == 1)
              List(other)
            else
              throw new ICE("""Cannot pattern match a tuple (%s)
                |to %s not of tuple type""".
                stripMargin.format(seq.prettyPrint, other.prettyPrint))
        }

        // Since tuples are guaranteed to be the right length by type
        // checking, we do not have to insert any checks here.
        unpackList(elems,
                   (index => {
                     val itemType = elemsTypesList(index)
                     val id = VariableGenerator.newTInternalVariable()
                     typeEnv.addTopLevel(id, itemType, false)
                     (TExpTupleExtract(TExpIdent(parentIdent), elems.length,
                                       index, id),
                      itemType)
                   }),
                   convertToAssignNodePat, typeEnv)
      }
      // Note that this case must be able to handle an empty list.
      case TListPat(listElems) => {
        val rawType = typeEnv.getOrFail(parentIdent) match {
          case TListType(typ) => typ
          case _ => throw new ICE("""Error cannot list pattern match
            | a non list type""".stripMargin)
        }
        // This is compiled to a check that the parent is of the right length.
        // If it is, then extract all this stuff and true.  Otherwise, false.
        val (assignExprs, idents) =
          unpackList(listElems,
                     (index => {
                       val id = VariableGenerator.newTInternalVariable()
                       typeEnv.addTopLevel(id, rawType, false)
                       (TExpListExtract(TExpIdent(parentIdent), index, id),
                        rawType)
                     }),
                     convertToAssignNodePat, typeEnv)

        val comparisonType = VariableGenerator.newTInternalVariable()

        // Insert the comparison type into the type environment
        typeEnv.addTopLevel(comparisonType,
                            TFunctionType(TTupleType(List(TIntType(),
                                                          TIntType())),
                                          TBoolType()),
                            false)

        (TExpIf(
          TExpFunApp(
            TExpIdent(TIntEqualsIdent()),
            TExpTuple(List(TExpConst(TConstInt(listElems.length)),
                           TExpListLength(TExpIdent(parentIdent)))),
            comparisonType),
          // There is no need for a true constant at the end of this.
          // It the assignExprs are true, then the match will be true.
          assignExprs,
          TExpConst(TConstFalse())),
         idents)
      }
      case TPatConst(const) => {
        val funIdent = VariableGenerator.newTInternalVariable()

        typeEnv.addTopLevel(funIdent,
                            TFunctionType(TTupleType(List(TBoolType(),
                                                          TBoolType())),
                                          TBoolType()),
                            false)

        val equalsIdent = typeEnv.getOrFail(parentIdent) match {
          case TBoolType() => TBoolEqualsIdent()
          case TRealType() => TRealEqualsIdent()
          case TIntType() => TIntEqualsIdent()
          case other => TGenericEqualsIdent()
        }

        (TExpFunApp(
          TExpIdent(equalsIdent),
          TExpTuple(List(TExpConst(const), TExpIdent(parentIdent))),
          funIdent), List())
      }
      case TPatWildcard() =>
        (TExpConst(TConstTrue()), List())
      case TPatCons(head, tail) => {
        // The idea is that this is transformed into the if statement:
        //
        //   if (parent.length > 0) then
        //      Assign(head, parent.head)
        //      Assign(tail, parent.tail)
        //      true
        //   else
        //      false

        // Do the same thing for the head and the tail:
        val headIdent = VariableGenerator.newTVariable(TValClass())
        val tailIdent = VariableGenerator.newTVariable(TValClass())
        val listTyIdent = VariableGenerator.newTInternalVariable()

        // Insert these types into the environment:
        val rawType = typeEnv.getOrFail(parentIdent) match {
          case TListType(typ) => typ
          case _ => throw new ICE("""
            |Trying to pattern match a list without a list type
            |as parent. """.stripMargin)
        }

        typeEnv.add(headIdent, rawType, false)
        typeEnv.add(tailIdent, TListType(rawType), false)
        typeEnv.addTopLevel(listTyIdent, rawType, false)

        val headExpression =
          TExpAssign(headIdent,
                     TExpListHead(TExpIdent(parentIdent), listTyIdent))
        val tailExpression =
          TExpAssign(tailIdent, TExpListTail(TExpIdent(parentIdent)))

        // Calculate the sub-expressions:
        val (headSubExprs, headSubIdents) =
          convertToAssignNodePat(headIdent, head, typeEnv)
        val (tailSubExprs, tailSubIdents) =
          convertToAssignNodePat(tailIdent, tail, typeEnv)

        // Add the type of the function call to the environment.
        val intCompareTypeID = VariableGenerator.newTInternalVariable()
        val intCompareType =
          TFunctionType(TTupleType(List(TIntType(), TIntType())),
                        TBoolType())

        typeEnv.addTopLevel(intCompareTypeID, intCompareType, false)

        val newExp =
          TExpIf(
            TExpFunApp(
              TExpIdent(TIntGTIdent()),
              TExpTuple(List(TExpListLength(TExpIdent(parentIdent)),
                             TExpConst(TConstInt(0)))),
              intCompareTypeID),
            // Then do the sub expressions.
            TExpSeq(List(headExpression, tailExpression,
                         combineExprsAnd(List(headSubExprs, tailSubExprs),
                                         typeEnv))).flatten,
            // Otherwise, do nothing but return false.
            TExpConst(TConstFalse()))
         (newExp,
          List(headIdent, tailIdent) ::: headSubIdents ::: tailSubIdents)
      }
    }

  /* This is a generic method that was introduced to deal with a lot of
   * repeated code in the various 'list' unpacking cases above.
   *
   * assignRHS should take the an index into  that item and return
   * a new TExp that represents
   * getting it out.  Returns a list of expressions and a list of new
   * introduced identifiers.  */
  def unpackList[T](elems: List[T], assignRHS: (Int) => (TExp, TType),
                    recursiveCall: (TNamedIdent, T, TTypeEnv) =>
                       (TExp, List[TNamedIdent]),
                    typeEnv: TTypeEnv) = {
    // Create a set of variables that can be used for each element.
    val intermediateIdents =
      elems.map(elem => VariableGenerator.newTVariable(TValClass()))

    var subElemsIdents = List[TNamedIdent]()
    // Now create a list of generation expressions that assign
    // these variables and use them to assign the sub-expressions:
    val computations =
      (intermediateIdents zip (elems zip (0 until elems.length))) map {
        case (assignIdent, (elem, index)) => {
          // Get the actual assignment expression:
          val (assignExp, assignType) = assignRHS(index)
          // Add the type for the assignIdent
          typeEnv.add(assignIdent, assignType, false)

          val (expressions, idents) = recursiveCall(assignIdent, elem, typeEnv)
          subElemsIdents = idents ::: subElemsIdents

          // First, assign to the identifier:
          TExpSeq(List(TExpAssign(assignIdent, assignExp),
            // Then add the assignments for the sub identifier
            expressions)).flatten
        }
      }

    val conditionalAssignment = combineExprsAnd(computations, typeEnv)

    (conditionalAssignment, intermediateIdents ::: subElemsIdents)
  }

  def combineExprsAnd(expr: List[TExp], typeEnv: TTypeEnv): TExp = {
    val funAppType = TFunctionType(
      TTupleType(List(TBoolType(), TBoolType())),
      TBoolType())
    expr.foldRight(TExpConst(TConstTrue()): TExp) {
      case (nextExp, currentExp) => {
        val boolFunctionTypeID = VariableGenerator.newTInternalVariable()
        val boolFunctionType =
          TFunctionType(TTupleType(List(TBoolType(), TBoolType())),
                        TBoolType())

        typeEnv.addTopLevel(boolFunctionTypeID, boolFunctionType, false)

        TExpFunApp(TExpIdent(TAnd()),
                   TExpTuple(List(nextExp, currentExp)),
                   boolFunctionTypeID)
      }
    }
  }

  /* This creates a list of assignment expressions that correspond
   * to assigning the variables to the patterns that they match.
   *
   * The expression is a boolean expression that evalueates
   * to true if the pattern matches and false otherwise.
   *
   * It inserts the TArgumentNode to represent identifiers
   * that come from indexed arguments.
   *
   * For 'case' expressions, the TArgumentNode s always point to
   * 1.
   */
  def generateAssignsFromPattern(pats: List[TPat], parents: List[TIdent],
                                 env: TTypeEnv) = {
    val (assignments, ident) = (pats zip parents).map {
      case (pat, argIdent) => convertToAssignNodePat(argIdent, pat, env)
    }.unzip

    (combineExprsAnd(assignments, env), ident)
  }
}
