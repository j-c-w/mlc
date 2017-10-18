package toplev

import org.rogach.scallop._

class Arguments(arguments: Seq[String]) extends ScallopConf(arguments) {
  val file = trailArg[String]()

  // Dump options
  val dumpAst = opt[Boolean]()
  val dumpTypecheck = opt[Boolean]()

  verify()
}
