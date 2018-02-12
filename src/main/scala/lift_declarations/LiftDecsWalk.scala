package lift_declarations

import exceptions.UnreachableException
import tir._
import tpass.TTypeEnvUpdateParentPass

class LiftDecsWalk extends TTypeEnvUpdateParentPass {
  var newTopDecs = List[TDataTypeDec]()

  override def apply(env: TTypeEnv, exp: TExp) = exp match {
    case TExpLetIn(decs, exp, env) => {
      val thisNewTopDecs =
        decs.filter(_.isInstanceOf[TDataTypeDec])
            .map(_.asInstanceOf[TDataTypeDec])
      val filteredDecs = decs.filter(!_.isInstanceOf[TDataTypeDec])

      // Need to add the lifted delcarations to the top level environments.
      thisNewTopDecs.foreach {
        case TDataTypeDec(name, args, typ) =>
          env.addTopLevel(name, env.getOrFail(name), false)
        case _ => throw new UnreachableException()
      }
      newTopDecs = thisNewTopDecs ::: newTopDecs

      Some(TExpLetIn(filteredDecs, exp, env))
    }
    case other => super.apply(env, exp)
  }
}
