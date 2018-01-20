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
    val runLowerAstVerify = opt[Boolean]()
    val runLambdaLiftVerify = opt[Boolean]()
    val runTInlineVerify = opt[Boolean]()
    val runLowerProgramVerify = opt[Boolean]()
    val runPreLowerSimplify = opt[Boolean]()

    // A marker to state whether the compiler should dump stats about
    // the compilation.
    val compileStats = opt[Boolean]()

    val optimize = opt[Boolean]()

    // Pass by pass optimization flags:
    val fTce = opt[Boolean]()
    val fnoTce = opt[Boolean]()

    val fTInline = opt[Boolean]()
    val fnoTInline = opt[Boolean]()

    val fPreLowerSimplify = opt[Boolean]()
    val fnoPreLowerSimplify = opt[Boolean]()

    val fPostLowerSimplify = opt[Boolean]()
    val fnoPostLowerSimplify = opt[Boolean]()

    val fSimplify = opt[Boolean]()
    val fnoSimplify = opt[Boolean]()

    val fByteDce = opt[Boolean]()
    val fnoByteDce = opt[Boolean]()

    val fPeephole = opt[Boolean]()
    val fnoPeephole = opt[Boolean]()

    // Dump options:
    val dumpAll = opt[Boolean]()
    val dumpLex = opt[Boolean]()
    val dumpAst = opt[Boolean]()
    val dumpChangeNames = opt[Boolean]()
    val dumpTypecheck = opt[Boolean]()
    val dumpTir = opt[Boolean]()
    val dumpTce = opt[Boolean]()
    val dumpLambdaLift = opt[Boolean]()
    val dumpTInline = opt[Boolean]()
    val dumpPreLowerSimplify = opt[Boolean]()
    val dumpLowerProgram = opt[Boolean]()
    val dumpSimplify = opt[Boolean]()
    val dumpNumberedProgram = opt[Boolean]()
    val dumpLowerTir = opt[Boolean]()
    val dumpByteDce = opt[Boolean]()
    val dumpPeephole = opt[Boolean]()

    verify()
  }

  val parser = new ArgumentsParser(arguments)

  val file = parser.file()
  val debug = parser.debug()
  
  // Optional pass options:
  val runLowerAstVerify = parser.runLowerAstVerify() || parser.verifyAll()
  val runTInlineVerify = parser.runTInlineVerify() || parser.verifyAll()
  val runLambdaLiftVerify = parser.runLambdaLiftVerify() || parser.verifyAll()
  val runLowerProgramVerify =
    parser.runLowerProgramVerify() || parser.verifyAll()

  // Stats
  val compileStats =
    parser.compileStats()

  val optimize =
    parser.optimize()

  // Pass run options:
  val runTce =
    parser.fTce() || (!parser.fnoTce() && optimize)
  val runTInline =
    parser.fTInline() || (!parser.fnoTInline() && optimize)
  val runSimplify =
    (optimize && !parser.fnoSimplify()) || parser.fSimplify()
  val runPreLowerSimplify =
    ((optimize || runSimplify) && !parser.fnoPreLowerSimplify()) ||
    parser.fPreLowerSimplify()
  val runPostLowerSimplify =
    ((optimize || runSimplify) && !parser.fnoPostLowerSimplify()) ||
    parser.fPostLowerSimplify()
  val runByteDce =
    (optimize && !parser.fnoByteDce()) || parser.fByteDce()
  val runPeephole =
    !parser.fnoPeephole() || parser.fPeephole()

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
  val dumpTce =
    parser.dumpTce() || parser.dumpAll()
  val dumpTInline =
    parser.dumpTInline() || parser.dumpAll()
  val dumpPreLowerSimplify =
    parser.dumpPreLowerSimplify() || parser.dumpAll()
  val dumpLowerProgram =
    parser.dumpLowerProgram() || parser.dumpAll()
  val dumpSimplify =
    parser.dumpSimplify() || parser.dumpAll()
  val dumpNumberedProgram =
    parser.dumpNumberedProgram() || parser.dumpAll()
  val dumpLowerTir =
    parser.dumpLowerTir() || parser.dumpAll()
  val dumpByteDce =
    parser.dumpByteDce() || parser.dumpAll()
  val dumpPeephole =
    parser.dumpPeephole() || parser.dumpAll()
}
