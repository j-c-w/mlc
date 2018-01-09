package recursion_analysis

import tir._
import tpass.TParentSetPass

/* This is an adaptation of the TParentSetPass that also keeps track
 * of whether current node is in a position to be a tail call or not.  */

abstract class TParentSetTailPass extends TParentSetPass[Boolean] {
  override def apply(isTail: Boolean, exp: TExp) = exp match {
    case TExpConst(_) | TExpIdent(_) | TExpContinue(_) =>
      super.apply(isTail, exp)
    case TExpFunApp(_, _, _) | TExpList(_) | TExpTuple(_) | TExpFn(_, _)
       | TExpAssign(_, _) | TExpListHead(_, _) | TExpListTail(_)
       | TExpTupleExtract(_, _, _, _) | TExpListExtract(_, _, _)
       | TExpListLength(_) | TExpThrow(_) | TExpBreak(_, _) =>
      super.apply(false, exp)
    case let @ TExpLetIn(decs, letExp, env) => {
      let.decs = getNew(let.decs, decs.map(apply(false, _)))
      let.exp = getNew(let.exp, apply(isTail, letExp))
      None
    }
    case whileLoop @ TExpWhile(cond, body, id) => {
      whileLoop.condition = getNew(cond, apply(isTail, cond))
      // In the current structure of while loops, the body could
      // indeed be the tail.
      whileLoop.body = getNew(body, apply(isTail, body))
      None
    }
    case exp @ TExpCase(caseExp, cases, appType) => {
      exp.exp = getNew(exp.exp, apply(false, caseExp))
      exp.cases =
        getNew(exp.cases,
               cases.map(apply(isTail, _).map(_.asInstanceOf[TExpMatchRow])))
      exp.applicationType =
        getNew(exp.applicationType,
               apply(false, appType).map(_.asInstanceOf[TInternalIdentVar]))
      None
    }
    case TExpSeq(elems) => {
      val newInit = getNew(elems.init, elems.init.map(apply(false, _)))
      val newLast = getNew(elems.last, apply(isTail, elems.last))

      Some(TExpSeq(newInit :+ newLast))
    }
    case matchRow @ TExpMatchRow(pat, exp, env) => {
      matchRow.pat = getNew(matchRow.pat, pat.map(apply(false, _)))
      matchRow.exp = getNew(matchRow.exp, apply(isTail, exp))
      None
    }
    case funLet @ TExpFunLet(valdecs, exp) => {
      val newVals =
        valdecs.map((x: TNamedIdent) =>
                    (x, apply(false, x).map(_.asInstanceOf[TNamedIdent])))

      setNew(newVals, valdecs)
      funLet.exp = getNew(funLet.exp, apply(isTail, funLet.exp))
      None
    }
    case ifStmt @ TExpIf(cond, ifTrue, ifFalse) => {
      ifStmt.cond = getNew(ifStmt.cond, apply(false, ifStmt.cond))
      ifStmt.ifTrue = getNew(ifStmt.ifTrue, apply(isTail, ifStmt.ifTrue))
      ifStmt.ifFalse = getNew(ifStmt.ifFalse, apply(isTail, ifStmt.ifFalse))
      None
    }
  }
}
