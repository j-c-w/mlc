package simplify

import exceptions.ICE
import tir._
import tpass._

/* The JVM does not accept code like this:
 *
 *    astore_1
 *    L2: aload_1
 *
 * Since we could skip the store before the load
 * (even if it never happens in practice!) Therefore, we use this pass to
 * eliminate redundant if statements.
 */

object TautologyWalk extends TParentSetPass[Unit] {
  def unreachable = throw new ICE("Untypeable case while walking TIR reached")

  override def apply(u: Unit, exp: TExp) = exp match {
    case app @ TExpFunApp(TExpIdent(TAnd()), _, _) => {
      // Walk the actual app first.  This removes any tautologies
      // from the arguments of the function, so all we have to do is check
      // for true and false
      val superResult = super.apply(u, exp)
      assert (superResult == None)

      app.application match {
        // Simplify C andalso Y
        case TExpTuple(List(TExpConst(c1), c2)) =>
          c1 match {
            case TConstTrue() => Some(c2)
            case TConstFalse() => Some(TExpConst(TConstFalse()))
            case _ => unreachable
          }
        // Simplify X andalso true
        case TExpTuple(List(c1, TExpConst(TConstTrue()))) =>
          Some(c1)
        // Simplify (X; Y; C) andalso (A; B; C') to something depending
        // on the values of C, C'
        case TExpTuple(List(TExpSeq(op1Elems :+ TExpConst(c1)),
                            TExpSeq(op2Elems :+ TExpConst(c2)))) => {
          val newAnd = (c1, c2) match {
            case (TConstFalse(), _) => TExpSeq(op1Elems).flatten
            case (_, TConstFalse()) =>
              TExpSeq(op1Elems ::: op2Elems).flatten
            case (TConstTrue(), TConstTrue()) =>
              TExpSeq(op1Elems ::: op2Elems ::: List(TExpConst(TConstTrue())))
            case _ => unreachable
          }

          // This is an enabler: also walk the sub expressions:
          Some(apply(u, newAnd).getOrElse(newAnd))
        }
        // Simplify (X; Y; Z) andalso (A) to
        // X; Y; (Z andalso A)
        case TExpTuple(List(TExpSeq(elems), op2)) => {
          val newExpression =
            TExpSeq(elems.init :+
                    TExpFunApp(TExpIdent(TAnd()),
                               TExpTuple(List(elems.last, op2)),
                               // The call type of this is still
                               // bool * bool -> bool
                               app.callType)).flatten

          val result = 
            apply(u, newExpression)

          Some(result.getOrElse(newExpression))
        }
        // Cannonicalize (if (...) then X else Y) andalso (Z)
        // -> (if (...) then (X andalso Z) else (Y andalso Z))
        // Note that the purpose of this is to enable optimizations
        // on the X andalso X and the Y andalso Z so do those.
        case TExpTuple(List(ifStmt @ TExpIf(cond, ifTrue, ifFalse), andAlso)) => {
          val newIfTrue = TExpFunApp(TExpIdent(TAnd()),
                                     TExpTuple(List(ifTrue, andAlso)),
                                     app.callType)
          val newIfFalse = TExpFunApp(TExpIdent(TAnd()),
                                      TExpTuple(List(ifFalse, andAlso)),
                                      app.callType)

          // Optimize those and put them in the new statement:
          Some(TExpIf(cond,
                      apply(u, newIfTrue).getOrElse(newIfTrue),
                      apply(u, newIfFalse).getOrElse(newIfFalse)))
        }
        case other => None
      }
    }
    case ifStmt @ TExpIf(_, _, _) => {
      // Walk the statement
      val superResult = super.apply(u, ifStmt)
      assert(superResult == None)

      ifStmt.cond match {
        // Simplify if C then X else Y
        case TExpConst(TConstTrue()) => Some(ifStmt.ifTrue)
        case TExpConst(TConstFalse()) => Some(ifStmt.ifFalse)
        // Simplify if ( =bool  (const) (exp))
        // -> if (exp), swapping the branches if needed.
        case TExpFunApp(TExpIdent(TBoolEqualsIdent()),
                        TExpTuple(List(TExpConst(constant), expression)),
                        callType) =>
          constant match {
            case TConstTrue() =>
              Some(TExpIf(expression, ifStmt.ifTrue, ifStmt.ifFalse))
            case TConstFalse() =>
              Some(TExpIf(expression, ifStmt.ifFalse, ifStmt.ifTrue))
            case _ => unreachable
          }
        // Canonicalize ( =bool (exp) (const)), swapping the branches
        // if needed.
        case TExpFunApp(TExpIdent(TBoolEqualsIdent()),
                        TExpTuple(List(expression, TExpConst(constant))),
                        callType) => {
          ifStmt.cond = TExpFunApp(TExpIdent(TBoolEqualsIdent()),
                                   TExpTuple(List(TExpConst(constant), expression)),
                                   callType)

          // Now we have flipped the condition, re-walk the if statement.
          apply(u, ifStmt)
        }
        // Simplify if (X; Y; C) then A else B to
        //      X; Y; if C then A else B
        // Recursively apply this to eliminate that if C is a constant.
        case TExpSeq(elements) => {
          val newExpression =
            TExpSeq(elements.init :+
                    TExpIf(elements.last, ifStmt.ifTrue,
                           ifStmt.ifFalse)).flatten

          Some(apply(u, newExpression).getOrElse(newExpression))
        }
        // Simplify: if (if (cond) then X; Y; true else A; B; false)
        //           then U else V
        // to
        //   if (cond) then X; Y; U else A; B; V
        case innerIf @ TExpIf(innerCond, innerTrue, innerFalse) => {
          val (endOfTrue, restOfTrue) = innerTrue match {
            case TExpSeq(elems) => (elems.last, elems.init)
            case other => (other, List[TExp]())
          }
          assert(!endOfTrue.isInstanceOf[TExpSeq])

          val (endOfFalse, restOfFalse) = innerFalse match {
            case TExpSeq(elems) => (elems.last, elems.init)
            case other => (other, List[TExp]())
          }
          assert(!endOfFalse.isInstanceOf[TExpSeq])

          val newStatement = (endOfTrue, endOfFalse) match {
            // if (if (X) then Y; true else Z; true) then A else B
            // -> (if X then Y else Z); A
            case (TExpConst(TConstTrue()), TExpConst(TConstTrue())) =>
              // Note that we cannot delete the boolean pushes from the
              // inner condition.
              Some(TExpSeq(List(innerIf, ifStmt.ifTrue)).flatten)
            // if (if (X) then Y; false else Z; false) then A else B
            // -> (if X then Y else Z); B
            case (TExpConst(TConstFalse()), TExpConst(TConstFalse())) =>
              Some(TExpSeq(List(innerIf, ifStmt.ifFalse)).flatten)
            // if (if (X) then Y; true else Z; false) then A else B
            // -> (if X then Y; A else Z; B)
            case (TExpConst(TConstTrue()), TExpConst(TConstFalse())) =>
              // We can take the condition out of the inner if statement and
              // put it as the main condition.  Then we have merge the true
              // and false branches.
              Some(TExpIf(innerIf.cond,
                          TExpSeq(List(innerIf.ifTrue,
                                       ifStmt.ifTrue)).flatten,
                          TExpSeq(List(innerIf.ifFalse,
                                       ifStmt.ifFalse)).flatten))
            // if (if (X) then Y; false else Z; true) then A else B
            // -> (if X then Y; B else Z; A)
            case (TExpConst(TConstFalse()), TExpConst(TConstTrue())) =>
              // We can do the reverse of the above
              Some(TExpIf(innerIf.cond,
                          TExpSeq(List(innerIf.ifFalse,
                                       ifStmt.ifTrue)).flatten,
                          TExpSeq(List(innerIf.ifTrue, ifStmt.ifFalse)).flatten
                          ))
            case other => None
          }

          newStatement
        }
        case other => None
      }
    }
    case seq @ TExpSeq(sequence) => {
      val flattened = super.apply(u, seq) match {
        case Some(seq : TExpSeq) => seq.flatten
        case Some(other) => other
        case None => seq.flatten
      }

      flattened match {
        case TExpSeq(seq) =>
          // Remove any 'useless' things, like loading constants
          // in the middle of expression sequences.
          val newList = (seq.init.filter {
            case TExpConst(_) => false
            case _ => true
          }).toList ++ List(seq.last)

          if (newList.length == 1) {
            Some(newList(0))
          } else {
            Some(TExpSeq(newList))
          }
        case other => Some(other)
      }
    }
    case other => super.apply(u, other)
  }

}
