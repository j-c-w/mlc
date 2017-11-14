package lambda_lift

import exceptions.ICE
import tir._
import tpass.TPass
import toplev.OptionalPass

/* This class contains a walk to the tree to make sure that:
 *   (1) there are no Fns left
 *   (2) there are no Fun decs left within let-in expressions.
 *
 * The actual integrity of the tree will be checked at a later date.  */

object LambdaLiftVerify extends OptionalPass[TProgram]("verify")
                        with TPass[Unit, Unit] {
  override def combine(x: Unit, y: Unit) = x
  override def default: Unit = ()

  override def apply(u: Unit, exp: TExp) = exp match {
    case tlet @ TExpLetIn(decs, innerExp, env) => {
      decs.foreach {
        case TFun(_, _) => throw new ICE("""Error, found a remaining
          |inner fundec in:
          |%s""".stripMargin.format(tlet.prettyPrint))
        case _ =>
      }

      // Then walk the rest of the let normally:
      super.apply(u, tlet)
    }
    case fndec @ TExpFn(_, _) => throw new ICE("""Error, found a
      |emaining fndec as
      |%s""".stripMargin.format(fndec.prettyPrint))
    case _ => super.apply(u, exp)
  }

  def run(tree: TProgram) = {
    apply((), tree)
    tree
  }
}
