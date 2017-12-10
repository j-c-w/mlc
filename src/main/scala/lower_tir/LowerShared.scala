package lower_tir

import scala.collection.mutable.{HashMap,Map}
import tir.TTopLevelIdent

object LowerShared {
  /* Functions are renamed, with one version created for each curried
   * application.  In order for locals to be able to access these functions,
   * they need to know the new class names.  This map provides those mappings.
   * Local variables may assume that the function name has been entered in
   * the map before they are required to load it.
   */
  val functionAccessMap: Map[TTopLevelIdent, String] =
    new HashMap[TTopLevelIdent, String]
}
