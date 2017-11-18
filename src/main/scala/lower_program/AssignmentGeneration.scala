package lower_program

import exceptions.ICE
import tir._
import typecheck.VariableGenerator

/* This is an object that provides helper methods for lowering
 * the program. 
 */
object AssignmentGeneration {
  def convertToAssignNodeDec(dec: TDec): (List[TExp], List[TIdentVar]) =
    dec match {
      case TVal(ident, exp) => {
        // The idea in this case is to assign the whole expression
        // to some variable, then extract it peice by peice.
        val tempIdent = VariableGenerator.newTVariable()

        val (exprs, ids) = convertToAssignNodeIdent(tempIdent, ident)

        (TExpAssign(tempIdent, exp) :: exprs, tempIdent :: ids)
      }
      case _ => throw new ICE("""Error: Cannot convert a TFun or a TJavaFun
        into an assign.  Statement was: %s""".format(dec.prettyPrint))
    }

  def convertToAssignNodeIdent(parentIdent: TIdent,
                               declaration: TIdent): (List[TExp],
                                                      List[TIdentVar]) =
    declaration match {
      case decIdent @ TIdentVar(name) => {
        (List(TExpAssign(decIdent, TExpIdent(parentIdent))),
         List(decIdent))
      }
      case TIdentTuple(subIdents) =>
        unpackList(subIdents, (index =>
            TExpTupleExtract(TExpIdent(parentIdent), index)),
                   convertToAssignNodeIdent)
      case TUnderscoreIdent() => (List(), List())
      case TUnitIdent() => (List(), List())
      case _ => throw new ICE("""Error, identifier %s was not expected
        |as an l-value""".stripMargin.format(declaration.prettyPrint))
    }

  /* Given some parent identifier from which to extract
   * the content, and some pattern, this function computes
   * a list of expressions that should be used to extract
   * a pattern
   */
  def convertToAssignNodePat(parentIdent: TIdent,
                             pat: TPat): (List[TExp], List[TIdentVar]) =
    pat match {
      case TPatVariable(identVar) =>
        (List(TExpAssign(identVar, TExpIdent(parentIdent))), List(identVar))
      case TPatIdentifier(TIdentVar(_)) => throw new ICE("""Error: A TIdentVar
        |in a TPatIdentifier should not occur""".stripMargin)
      case TPatIdentifier(_) => (List(), List())
      case TPatSeq(elems) =>
        unpackList(elems, (index =>
                                TExpTupleExtract(TExpIdent(parentIdent), index)),
                   convertToAssignNodePat)
      case TListPat(listElems) =>
        unpackList(listElems, (index =>
                                TExpListExtract(TExpIdent(parentIdent), index)),
                   convertToAssignNodePat)
      case TPatConst(_) => (List(), List())
      case TPatWildcard() => (List(), List())
      case TPatCons(head, tail) => {
        // Do the same thing for the head and the tail:
        val headIdent = VariableGenerator.newTVariable()
        val tailIdent = VariableGenerator.newTVariable()

        val headExpresssion =
          TExpAssign(headIdent, TExpListHead(TExpIdent(parentIdent)))
        val tailExpresssion =
          TExpAssign(tailIdent, TExpListTail(TExpIdent(parentIdent)))

        // Calculate the sub-expressions:
        val (headSubExprs, headSubIdents) =
          convertToAssignNodePat(headIdent, head)
        val (tailSubExprs, tailSubIdents) =
          convertToAssignNodePat(tailIdent, tail)

        (((headExpresssion :: headSubExprs) :::
          (tailExpresssion :: tailSubExprs)),
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
  def unpackList[T](elems: List[T], assignRHS: (Int) => TExp,
                       recursiveCall: (TIdentVar, T) =>
                         (List[TExp], List[TIdentVar])) = {
    // Create a set of variables that can be used for each element.
    val intermediateIdents =
      elems.map(elem => VariableGenerator.newTVariable())

    var subElemsIdents = List[TIdentVar]()
    // Now create a list of generation expressions that assign
    // these variables and use them to assign the sub-expressions:
    val assignments =
      (intermediateIdents zip (elems zip (0 until elems.length))) map {
        case (assignIdent, (elem, index)) => {
          val (expressions, idents) = recursiveCall(assignIdent, elem)
          subElemsIdents = idents ::: subElemsIdents

          // First, assign to the identifier:
          TExpAssign(assignIdent, assignRHS(index)) ::
            // Then add the assignments for the sub identifier
            expressions
        }
      }

    (assignments.flatten, intermediateIdents ::: subElemsIdents)
  }

  /* This creates a list of assignment expressions that correspond
   * to assigning the variables to the patterns that they match.
   *
   * It inserts the TArgumentNode to represent identifiers
   * that come from indexed arguments.
   *
   * For 'case' expressions, the TArgumentNode s always point to
   * 1.
   */
  def generateAssignsFromPattern(pats: List[TPat]) = {
    var number = -1
    // Create a pair of argument numbers and identifiers.
    val argNumberPatternPairs =
      pats.map(x => { number += 1; (x, TArgumentNode(number))} )

    argNumberPatternPairs.map {
      case (pat, argIdent) => convertToAssignNodePat(argIdent, pat)
    }
  }
}
