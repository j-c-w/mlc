package toplev

import org.rogach.scallop._

class Arguments(arguments: Seq[String]) extends ScallopConf(arguments) {
  val file = trailArg[String]()
  // This is intended to be used to see the full
  // stack traces of internal errors.
  val debug = opt[Boolean]()

  // Dump options
  val dumpAst = opt[Boolean]()
  val dumpTypecheck = opt[Boolean]()

  verify()
}