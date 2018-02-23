package change_names

import scala.collection.mutable.Map
import tir._
import tpass.TTypeEnvUpdateParentPass

/* This class implements a generic walk that changes identifier
 * names as it goes.
 */

class ChangeIdentNamesWalk(namesToReplace: Map[TNamedIdent,
                                               (TNamedIdent, TType)])
    extends TTypeEnvUpdateParentPass {
  override def apply(typeEnv: TTypeEnv, ident: TIdent) = ident match {
    case ident : TNamedIdent => {
      if (namesToReplace.contains(ident)) {
        val (newName, identType) = namesToReplace(ident)

        // Also ensure that the new variable is in the type environment here:
        if (!typeEnv.hasType(newName))
          typeEnv.add(newName, identType, false)

        Some(newName)
      } else {
        None
      }
    }
    case other => super.apply(typeEnv, other)
  }

  /* We also ensure that the new variables are inserted into the same
   * envs that their parents are inserted into.
   */
  override def onTouchEnv(env: TTypeEnv) = {
    namesToReplace.foreach {
      case (oldIdent, (newIdent, typ)) => {
        if (!env.hasType(newIdent)
            && env.hasType(oldIdent)) {
          // Also sanity check that the types are the same.
          assert(env.getOrFail(oldIdent) == typ)
          env.swapNames(oldIdent, newIdent)
        }
      }
    }
  }
}
