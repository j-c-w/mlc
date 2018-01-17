package simplify

import exceptions.ICE
import tpass.TParentSetPass
import tir._

/* This walk should only be run after lambda lifting
 * and before lowering program.  It ccan clean up from optimization passes
 * very well.  */

object CaseWalk extends TParentSetPass[Unit] {
  override def apply(u: Unit, exp: TExp) = exp match {
    // We can replace let statements that expand tuples
    // with a sequence of value expressions.  This allows
    // other  passes to optimize better.
    // e.g.
    //  let
    //    val (x, y) = (1, 2)
    //  in ... end
    // to
    //  let
    //   val x = 1
    //   val y = 2
    //  in ... end
    case TExpLetIn(decs, exp, env) => {
      val newDecs = decs.flatMap {
        case TVal(TIdentTuple(idents), TExpTuple(elems)) => {
          assert(idents.length == elems.length)
          (idents zip elems) map {
            case (ident: TIdent, exp: TExp) => TVal(ident, exp)
          }
        }
        case other => List(other)
      }

      val newLet = TExpLetIn(newDecs, exp, env)
      super.apply((), newLet)
      Some(newLet)
    }
    // To enable the simplification of this as above, convert to a let
    // if possible:
    case TExpCase(caseExp, patterns, typ) => {
      if (patterns.length == 1) {
        val TExpMatchRow(pat, patExp, env) = patterns(0)
        // The pattern must be of length 1 as case statements may not be
        // curried.
        assert(pat.length == 1)

        if (isSimplePattern(pat(0))) {
          val newExp = TExpLetIn(List(TVal(convertToValPat(pat(0)), caseExp)),
                                 patExp, env)
          Some(apply((), newExp).getOrElse(newExp))
        } else {
          None
        }
      } else {
        None
      }
    }
    case other => super.apply((), other)
  }

  /* This function checks whether a pattern is  comptable with a val
   * match.  This is expected to change (and eventually be removed
   * as val matches are expanded to a full language).
   */
  private def isSimplePattern(pat: TPat): Boolean = pat match {
    case TPatWildcard() => true
    case TPatSeq(subSeq) =>
      subSeq.forall(isSimplePattern(_))
    case TPatVariable(_) => true
    case other => false
  }

  private def convertToValPat(pat: TPat): TIdent = pat match {
    case TPatWildcard() => TUnderscoreIdent()
    case TPatSeq(subSeq) => TIdentTuple(subSeq.map(convertToValPat(_)))
    case TPatVariable(name) => name
    case other =>
      throw new ICE("Cannot change pattern with %s".format(other.prettyPrint))
  }
}
