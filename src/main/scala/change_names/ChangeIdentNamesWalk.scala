package change_names

import scala.collection.mutable.Map
import tir._
import tpass.TTypeEnvUpdateParentPass

/* This class implements a generic walk that changes identifier
 * names as it goes.
 */

class ChangeIdentNamesWalk(namesToReplace: Map[TIdentVar, (TIdentVar, TType)])
    extends TTypeEnvUpdateParentPass {
  override def applyIdentVar(typeEnv: TTypeEnv, ident: TIdentVar) = {
    if (namesToReplace.contains(ident)) {
      val (newName, identType) = namesToReplace(ident)

      // Also ensure that the new variable is in the type environment here:
      if (!typeEnv.hasType(newName)) {
        typeEnv.add(newName, identType, false)
      }

      Some(newName)
    } else {
      None
    }
  }
}