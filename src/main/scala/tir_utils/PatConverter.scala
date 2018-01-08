package tir_utils

import exceptions.ICE
import tir._

/* This class deals with conversions between different representations of tree
 * patterns.  */

object PatConverter {

  /* Given some pattern of the form:
   *  x y z => e
   * Convert it to a pattern of the form:
   *
   *  (x, y, z) => e.
   */
  def decurry(curriedList: List[TPat]): TPat =
    if (curriedList.length == 0) {
      throw new ICE("Attempting to decurry list of length 0")
    } else if (curriedList.length == 1) {
      curriedList(0)
    } else {
      TPatSeq(curriedList)
    }
}
