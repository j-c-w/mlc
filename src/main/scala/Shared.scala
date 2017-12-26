package toplev

import target.TargetConfig

/* This file contains some shared information throught the compiler.
 * 
 * To keep sanity, all items in this file MUST be constant
 * throught the compilation process of a file.
 */

object Shared {
  var filename: String = null
  var debug: Boolean = false

  // This is stored here to avoid the mistake of GCC and allow this compiler
  // to run for any backend target without too much difficulty.
  var targetConfig: TargetConfig = null
}
