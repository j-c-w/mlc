package toplev

import java.io.File
import java.nio.charset.StandardCharsets

import utils.FileUtils

// These passes are imported in the order they are used.
import frontend.GLLParser
import ast_change_names.ASTChangeNames
import typecheck.HindleyMilner
import lower_ast.LowerAST
import lambda_lift.LambdaLift
import lambda_lift.LambdaLiftVerify
import lower_program.LowerProgram
import lower_program.LowerProgramVerify
import lower_variables.LowerVariablesPass
import lower_tir.LowerTIR

object Toplev extends App {
  val startTime = System.currentTimeMillis()
  val cli = new Arguments(args)
  val file = new File(cli.file)

  if (!file.exists) {
    println("Error, file " + file.toString + " doesn't exist")
    System.exit(1)
  }

  Shared.filename = file.toString
  Shared.debug = cli.debug
  Shared.verifyUnifiers = cli.runVerifyUnifiers

  // Frontend
  val code = FileUtils.readFileToString(file, StandardCharsets.UTF_8)
  val tree = GLLParser.execute(code, cli.dumpAst)
  val uniqueified = ASTChangeNames.execute(tree, cli.dumpChangeNames)
  val typechecked = HindleyMilner.execute(uniqueified, cli.dumpTypecheck)

  // Lowering from AST -- These all use the tree and so are OK
  // on memory.
  val intermediate = LowerAST.execute(typechecked, cli.dumpTir)
  val lambda_lifted = LambdaLift.execute(intermediate, cli.dumpLambdaLift)
  val _0 = LambdaLiftVerify.optionalExecute(cli.runLambdaLiftVerify,
                                            lambda_lifted, false)

  // Optimizations on the TIR

  // Lower the TIR down into TIR+Assigns.
  val lowered_program = LowerProgram.execute(lambda_lifted,
                                             cli.dumpLowerProgram)
  val _1 = LowerProgramVerify.optionalExecute(cli.runLowerProgramVerify,
                                              lowered_program, false)

  // Optimizations on TIR+Assigns

  // Lower TIR+Assigns into byteR
  val numberedProgram = LowerVariablesPass.execute(lowered_program,
                                                   cli.dumpNumberedProgram)

  val byteR = LowerTIR.execute(numberedProgram, cli.dumpLowerTir)

  // Optimizations on byteR
  
  // Output byteR
  val outputFileName = Shared.filename.replaceAll("\\.[^.]*$", "") + ".j"
  FileUtils.writeStringToFile(outputFileName, byteR.prettyPrint)

  if (cli.compileStats) {
    val endTime = System.currentTimeMillis()

    println("Compile time: " + (endTime - startTime) + "ms")
  }
}
