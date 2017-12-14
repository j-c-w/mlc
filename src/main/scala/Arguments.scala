package toplev

import org.rogach.scallop._

class Arguments(arguments: Seq[String]) {
  class ArgumentsParser(args: Seq[String]) extends ScallopConf(args) {
    val file = trailArg[String]()
    // This is intended to be used to see the full
    // stack traces of internal errors.
    val debug = opt[Boolean]()

    // Optional pass options:
    val verifyAll = opt[Boolean]()
    val runLambdaLiftVerify = opt[Boolean]()
    val runLowerProgramVerify = opt[Boolean]()

    // A marker to state whether the compiler should dump stats about
    // the compilation.
    val compileStats = opt[Boolean]()

    // Dump options:
    val dumpAll = opt[Boolean]()
    val dumpLex = opt[Boolean]()
    val dumpAst = opt[Boolean]()
    val dumpChangeNames = opt[Boolean]()
    val dumpTypecheck = opt[Boolean]()
    val dumpTir = opt[Boolean]()
    val dumpLambdaLift = opt[Boolean]()
    val dumpLowerProgram = opt[Boolean]()
    val dumpSimplify = opt[Boolean]()
    val dumpNumberedProgram = opt[Boolean]()
    val dumpLowerTir = opt[Boolean]()

    verify()
  }

  val parser = new ArgumentsParser(arguments)

  val file = parser.file()
  val debug = parser.debug()
  
  // Optional pass options:
  val runLambdaLiftVerify = parser.runLambdaLiftVerify() || parser.verifyAll()
  val runLowerProgramVerify =
    parser.runLowerProgramVerify() || parser.verifyAll()

  // Stats
  val compileStats =
    parser.compileStats()

  // Dump options:
  val dumpLex =
    parser.dumpLex() || parser.dumpAll()
  val dumpAst =
    parser.dumpAst() || parser.dumpAll()
  val dumpChangeNames =
    parser.dumpChangeNames() || parser.dumpAll()
  val dumpTypecheck =
    parser.dumpTypecheck() || parser.dumpAll()
  val dumpTir =
    parser.dumpTir() || parser.dumpAll()
  val dumpLambdaLift =
    parser.dumpLambdaLift() || parser.dumpAll()
  val dumpLowerProgram =
    parser.dumpLowerProgram() || parser.dumpAll()
  val dumpSimplify =
    parser.dumpSimplify() || parser.dumpAll()
  val dumpNumberedProgram =
    parser.dumpNumberedProgram() || parser.dumpAll()
  val dumpLowerTir =
    parser.dumpLowerTir() || parser.dumpAll()
}
